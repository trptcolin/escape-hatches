require "active_record"

def setup_database
  require "sqlite3"
  ActiveRecord::Base.establish_connection(
    :adapter => "sqlite3",
    :database => ":memory:"
  )

  ActiveRecord::Schema.define do
    create_table :users, force: true do |t|
      t.string :email
      t.string :activation_code
      t.boolean :active
      t.index :email, :unique => true
      t.index :activation_code, :unique => true
    end
  end
end

class User < ActiveRecord::Base
end

class ActivateUser
  def call(activation_code)
    user = fetch_user(activation_code)
    raise ArgumentError, "Activation code '#{activation_code}' is invalid" if !user
    user.update!(:activation_code => nil, :active => true)
    user
  end

  private
  def fetch_user(activation_code)
    User.where(:activation_code => activation_code).first
  end
end

require "minitest"

class TestActivateUser < Minitest::Test
  def setup
    User.delete_all
    User.create(:email => "jen@example.com", :activation_code => "GOOD_CODE")
    @activator = ActivateUser.new
  end

  def test_a_bad_activation_code_cant_be_fetched
    assert_raises(ArgumentError) { @activator.call("BAD_CODE") }
  end

  def test_a_good_activation_code_updates_and_returns_the_user
    user = @activator.call("GOOD_CODE")
    assert_equal user, User.where(:email => user.email).first
    assert_equal "jen@example.com", user.email
    assert_nil user.activation_code
    assert_equal true, user.active
  end

  def test_a_good_activation_code_updates_the_user
    user = @activator.call("GOOD_CODE")
    assert_equal "jen@example.com", user.email
    assert_nil user.activation_code
    assert_equal true, user.active
  end

  def test_private_method_access_prevented
    assert_raises(NoMethodError, /private method `fetch_user` called for/) {
      @activator.fetch_user("GOOD_CODE")
    }
  end

  def test_private_method_access_allowed_with_send
    user = @activator.send(:fetch_user, "GOOD_CODE")
    assert_equal "jen@example.com", user.email
  end

  def test_private_methods_can_be_made_public
    # We could send this message to `ActivateUser` instead, but then we'd
    # need `Minitest::Test.i_suck_and_my_tests_are_order_dependent!` to keep
    # our tests deterministic, and nobody wants that.
    @activator.singleton_class.send(:public, :fetch_user)

    user = @activator.fetch_user("GOOD_CODE")
    assert_equal "jen@example.com", user.email
  end
end

if $0 == __FILE__
  setup_database
  require "minitest/autorun"
end

# $ ruby ./private.rb
