# scs 1.0.0 (2022-12-22)

## Major changes

* `data` folder renamed to `survey-data`.

* New functions added: 
   * `backup_data` copies data files to a zipped folder.
   * `scs_filepath` constructs filepaths for use across.
   * `update_email` updates a participant's email address in registration data.
   
* Built-in datasets added: 
   * `resp_names`,
   * `dummy_reg`,
   * `dummy_resp`,
   * `dummy_opt_outs`.

* `anon_reg_data` and `anon_response_data` superceded by `anonymise_data`.


## Minor improvements

* `remove_opt_outs` function removed.
* `delete_files` now requires confirmation via pop up window instead of console interaction. This requirement can be switched off using `user_confirm` argument.
* `dplyr::across` used instead of scoped helpers.
* More checks added to produce errors/warnings where necessary.
* Improved unit testing coverage.


# scs 0.1.0 (2022-09-15)

* Initial package release.
* Data cleaning and processing functions added.
