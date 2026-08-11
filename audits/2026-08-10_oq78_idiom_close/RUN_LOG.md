# OQ-78 idiom close — RUN LOG

## FREEZE
prereg_frozen_at:  2026-08-11T03:48:51Z
prereg_md5:        384e68bbac80e0959dba1294a6f6ee87
prereg_bytes:      16841
code_commit:       d5f4d006ddd6bcda70e08145a06d82d95e4d2df6
instrument_md5:    bdf29526858eee7e823604d96125581e
frozen_default_leg_md5: 46386b33de3bddbbbb2bd5f4e6d7478f
corpus fingerprints (test + calibration legs):
  testsets_haiku  f697246d3331b4528e6f1b2591ae5b5c
  testsets_flash  6c6a2dbd832f33031441e286089e3dd6
  testsets_kimi  57d485238b4c33bf604c896ff3ebcec7
  testsets_sonnet  2427448c1b3c7d6e4b607cb883d3918c

## FIRST SONNET-5 RESULT LINE FOLLOWS BELOW THIS POINT

### TEST READ — sonnet-5 (claude-sonnet-5), n=1001

CONDITION (ii) PRECONDITION: worst_pair_auc = 0.8858  vs threshold 0.6347  -> HOLDS
   pairwise AUC: {'rope<tangled_rope': 0.9597, 'tangled_rope<snare': 0.8858}

CONDITION (i) POOLED: argmax=8 share=0.4310 conc=0.3310 (floor 0.25) off_grid=0.0080 share_at_8=0.4310
   pooled digit histogram: {'0': 6, '1': 132, '2': 355, '3': 11, '4': 8, '5': 14, '6': 26, '7': 7, '8': 428, '9': 6}
   pooled below floor (i.e. (i) SATISFIED)? -> False

CONDITION (i) PER TYPE (min scored cell 50):
   tangled_rope  n= 690 argmax=8 share=0.4161 conc=0.3161 at8=0.4161  fires=True
   rope          n= 165 argmax=8 share=0.5273 conc=0.4273 at8=0.5273  fires=True
   snare         n=  59 argmax=1 share=0.4386 conc=0.3386 at8=0.2807  fires=True
   excluded below n=50: {'scaffold': 24, 'mountain': 34, 'piton': 29}

PAIRED PRIMARY (4-way matched, n=957): tv_model_digit=0.3654 floor=0.15 null_p99=0.03814 -> fires=True
   all_agree_rate = 0.0125   model_bound = True
   haiku   argmax=8 share=0.6249 conc=0.5249 at8=0.6249
   flash   argmax=5 share=0.6228 conc=0.5228 at8=0.0293
   kimi    argmax=2 share=0.3804 conc=0.2804 at8=0.2727
   sonnet  argmax=8 share=0.4274 conc=0.3274 at8=0.4274

SECONDARY (default-leg sonnet-5, pooled only, n=95): argmax=8 share=0.4421 conc=0.3421 at8=0.4421  concordant_with_test=True

TEST BANDS:
   mountain      n=  34 p10=0.040 med=0.110 p90=0.532 distinct=17
   rope          n= 165 p10=0.080 med=0.220 p90=0.380 distinct=23
   scaffold      n=  24 p10=0.280 med=0.410 p90=0.462 distinct=10
   piton         n=  29 p10=0.180 med=0.480 p90=0.710 distinct=12
   tangled_rope  n= 690 p10=0.418 med=0.580 p90=0.710 distinct=34
   snare         n=  59 p10=0.580 med=0.790 p90=0.880 distinct=17

### DESCRIPTIVES (post-hoc, NOT pinned conditions)

population                         n     .x8     .x2   x8+x2   ε=0.68  distinct
leg_haiku                        960   62.3%   19.8%   82.1%    31.8%        42
leg_flash                        960    2.9%    0.5%    3.4%     1.6%        30
leg_kimi                        1005   27.7%   38.1%   65.8%     5.3%        68
leg_sonnet                      1001   42.8%   35.5%   78.2%     7.3%        52
archive_kernel_v2_test2           60   76.7%   15.0%   91.7%    30.0%        13
default_derived_sonnet4.5         64   76.6%   20.3%   96.9%    50.0%        12

Historical baselines from ISSUES.md OQ-78: n=91 build -> .x8 86%, ε=0.68 34%, 13 distinct;
                                            n=60 archive -> .x8 77%, ε=0.68 30%, 13 distinct.

### TYPE-VOCABULARY DELTA (test stratum vs archive comparator)
claimed_type      sonnet-5 (n=1001)     archive (n=60)
mountain             34 ( 3.4%)        9 (15.0%)
piton                29 ( 2.9%)        1 ( 1.7%)
rope                165 (16.5%)       10 (16.7%)
scaffold             24 ( 2.4%)        0 ( 0.0%)
snare                59 ( 5.9%)       17 (28.3%)
tangled_rope        690 (68.9%)       23 (38.3%)

### institutional_trust_erosion_c0 follow-up
  present in frozen default leg: [('institutional_trust_erosion_c0', 0.68, 'mountain')]
  mountain cell in TEST stratum: n=34 -> BELOW the min scored cell of 50, so per-type
  localization for mountain is NOT licensed in this stratum; the band-break is
  UNFOLLOWABLE here (stated as such, not 'not promoted').
