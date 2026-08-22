% ============================================================================
% CONSTRAINT STORY: family_law_authority__secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__secular_contractual_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: family_law_authority__secular_contractual_reading
 *   human_readable: Marriage as Civil Contract under State Law (Secular Contractual Reading)
 *   domain: legal/political/social
 *
 * SUMMARY:
 *   This constraint story instantiates the secular-contractual reading of the
 *   family-law-authority kernel: marriage validity derives from individual
 *   consent and state registration, independent of religious sanction or
 *   family gatekeeping. The reading vindicates three propositions—individual
 *   autonomy, gender equality before law, and state monopoly on marriage
 *   validity—but the constraint itself is lower-extraction than its
 *   religious-authority siblings because it claims to operate on consensual,
 *   symmetric grounds with minimal coercive overhead. The measurement series
 *   show stable low-to-moderate extractiveness and theater ratio across the
 *   interval, indicating the constraint's coordination function is durable
 *   and the performative load is light. Suppression declines slightly in the
 *   mid-interval as normalization sets in, then stabilizes. This is a rope
 *   reading of the kernel: the constraint solves a genuine coordination
 *   problem (standardizing pair-bonding across religious pluralism) and the
 *   main beneficiaries are also net-positive participants (women gain
 *   autonomy, interfaith couples gain access). The claim and metrics are
 *   aligned for this reading—it genuinely claims lower extraction than the
 *   religious-authority siblings. Committer frame: this constraint is one of
 *   five kernel readings; the others (Hindu dharmaśāstra, Muslim sharīʿah,
 *   Christian canonical, Parsi Zoroastrian) will each author their own ε
 *   values and beneficiary/victim sets grounded in their respective authority
 *   structures and readings of the founding problem.
 *
 * KEY AGENTS:
 *   - State registration authority: institutional agenda-setter, administers validity criteria and enforces gender-symmetric contract law; collects administrative fees and maintains marital-status records
 *   - Women as autonomous agents: moderate-power beneficiaries; gain equal contractual capacity, property retention, and divorce rights; exit options shift from trapped (under patriarchal family law) to mobile
 *   - Interfaith couples: moderate-power beneficiaries; access marriage validity across religious boundaries without requiring conversion or religious authority approval
 *   - Men as contract principals: moderate-power beneficiaries under this reading specifically; placed on symmetric grounds with women rather than asymmetric authority
 *   - Religious authorities: institutional excluded stakeholders; lose legal gatekeeping over marriage validity; their role reduces to solemnization or pastoral counsel
 *   - Families as custodians: moderate-power excluded stakeholders; lose legal authority to arrange, consent to, or dissolve marriages; displaced by individual autonomy principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, 0.38).
domain_priors:suppression_score(family_law_authority__secular_contractual_reading, 0.22).
domain_priors:theater_ratio(family_law_authority__secular_contractual_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, accessibility_collapse, 0.31).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__secular_contractual_reading, rope).
narrative_ontology:human_readable(family_law_authority__secular_contractual_reading, "Marriage as Civil Contract under State Law (Secular Contractual Reading)").
narrative_ontology:topic_domain(family_law_authority__secular_contractual_reading, "legal/political/social").

domain_priors:requires_active_enforcement(family_law_authority__secular_contractual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__secular_contractual_reading, '3a6fbdea-bff3-4d91-b156-1f4d0776a890').
narrative_ontology:cs_kernel_codification('3a6fbdea-bff3-4d91-b156-1f4d0776a890', formalized).
narrative_ontology:cs_authority_grounding('3a6fbdea-bff3-4d91-b156-1f4d0776a890', expertise).
narrative_ontology:cs_interpretation_layer_present('3a6fbdea-bff3-4d91-b156-1f4d0776a890').
narrative_ontology:cs_reading_relation('3a6fbdea-bff3-4d91-b156-1f4d0776a890', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a6fbdea-bff3-4d91-b156-1f4d0776a890', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a6fbdea-bff3-4d91-b156-1f4d0776a890', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a6fbdea-bff3-4d91-b156-1f4d0776a890', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('3a6fbdea-bff3-4d91-b156-1f4d0776a890', foundational, individual_autonomy_in_marriage_contract).
narrative_ontology:cs_axiom_status(individual_autonomy_in_marriage_contract, holdable).
narrative_ontology:cs_axiom_grounding('3a6fbdea-bff3-4d91-b156-1f4d0776a890', individual_autonomy_in_marriage_contract, deontological).
narrative_ontology:cs_axiom('3a6fbdea-bff3-4d91-b156-1f4d0776a890', foundational, gender_equality_before_state_law).
narrative_ontology:cs_axiom_status(gender_equality_before_state_law, holdable).
narrative_ontology:cs_axiom_grounding('3a6fbdea-bff3-4d91-b156-1f4d0776a890', gender_equality_before_state_law, conventional).
narrative_ontology:cs_axiom('3a6fbdea-bff3-4d91-b156-1f4d0776a890', secondary, state_monopoly_on_legal_marriage_validity).
narrative_ontology:cs_axiom_status(state_monopoly_on_legal_marriage_validity, holdable).
narrative_ontology:cs_axiom_grounding('3a6fbdea-bff3-4d91-b156-1f4d0776a890', state_monopoly_on_legal_marriage_validity, conventional).
narrative_ontology:cs_reference_frame('3a6fbdea-bff3-4d91-b156-1f4d0776a890', secular_legal_contractualism).
narrative_ontology:cs_drift_state('3a6fbdea-bff3-4d91-b156-1f4d0776a890', contemporary_human_rights_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3a6fbdea-bff3-4d91-b156-1f4d0776a890', '').
narrative_ontology:cs_kernel_id(family_law_authority__secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, state_registration_authority).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, women_autonomous_agents).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, interfaith_couples).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, men_contract_principals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers marriage registration and contract enforcement, sets validity criteria, enforces gender-symmetric rights through family law statutes. Claims authority grounded in civil law tradition and constitutional guarantees of equality. Collects administrative fees and maintains the authoritative record of marital status.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, state_registration_authority, agenda_setter,
    institutional, generational, analytical, national).

% Holds equal legal capacity to enter, modify, and exit marriage contracts; retains separate legal personhood, property rights, and testamentary capacity; can divorce on equal grounds with men. This reading grounds their legal capacity in individual autonomy rather than family honor or dharmic duty.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, women_autonomous_agents, beneficiary,
    moderate, biographical, mobile, national).

% Can marry across religious boundaries without requiring conversion or religious authority blessing; validity derives solely from state registration, not from religious law or community recognition. Their marriage exists as a legal fact independent of whether any religious tradition recognizes it.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, interfaith_couples, beneficiary,
    moderate, biographical, mobile, national).

% Hold equal contractual capacity, property co-ownership rights, and guardianship authority under this reading, placed on symmetry with women rather than asymmetric rights. Exit marriage through divorce on equal grounds. The reading treats gender as administratively neutral to contract validity.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, men_contract_principals, beneficiary,
    moderate, biographical, mobile, national).

% Are structurally excluded from adjudicating marriage validity under this reading; their role is reduced to solemnizing ceremonies or pastoral counseling, without legal authority. Their objection—that marriage is a sacred act not a secular contract—is the core disagreement with this reading but they hold no veto over state-registered marriages.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_authorities, excluded,
    institutional, generational, constrained, national).

% Under this reading, families lose the legal authority to arrange, consent to, or dissolve marriages; the individuals alone hold contract capacity. Families retain emotional and social roles but no legal power to override the autonomous contract, which is the structural displacement this reading embodies relative to its siblings.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, families_as_custodians, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__secular_contractual_reading, state_registration_authority).
narrative_ontology:fixing_cost_class(family_law_authority__secular_contractual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Registers and enforces consensual pair-bonding contracts, standardizing property co-ownership, inheritance, tax treatment, and custody arrangements across a pluralistic population without requiring religious uniformity or family mediation.
% TRANSFER_FUNCTION: Redistributes marital property and custody authority from families and religious authorities to autonomous individuals; validates gender-symmetric legal capacity where previously asymmetry was encoded in family law. The constraint moves authority over marriage validity from religious institutions and patriarchal kinship structures into the civil sphere.
% ABSENT_VOICES: Religious authorities who hold that marriage is a sacrament outside state jurisdiction, and traditional family structures for whom the constraint strips their gatekeeping power over marital legitimacy. They would argue marriage validity should derive from religious law or family consent, not state registration alone—but this reading structurally excludes that voice from legal authority.
% DISAPPEARANCE_RATIONALE: If state registration ceased to confer marital validity, couples would revert to religious or customary ceremonies for legitimacy, interfaith marriages would lose legal recognition, women's property and guardianship rights would revert to family control in many contexts, and inheritance and tax law would collapse without a state-recognized marriage fact. The secular legal order of marriage would reorganize around religious or patriarchal authority structures.
% FOUNDING_PROBLEM: Religious fragmentation and patriarchal family authority prevented equal legal capacity for women and structurally barred interfaith marriages. Individuals lacked contractual autonomy over the choice and dissolution of marriage.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts and human-rights bodies (outside the reading's benefiting parties) attest the founding problem is live in jurisdictions where religious or customary law retains primacy. Feminist jurisprudence independent of state interest corroborates the autonomy claim. Religious authorities themselves contest the problem statement, arguing patriarchal structure and religious gatekeeping are features, not bugs—their testimony from outside the benefiting parties contradicts the founding problem's framing rather than confirming it.
narrative_ontology:disappearance_verdict(family_law_authority__secular_contractual_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__secular_contractual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__secular_contractual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__secular_contractual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__secular_contractual_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__secular_contractual_reading_tests).
:- end_tests(family_law_authority__secular_contractual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the state retains registration authority and enforces legal consequences (tax, inheritance, custody) conditional on compliance—this is genuine state power. However, extractiveness is lower than religious-authority readings because the constraint claims to operate on consensual symmetric grounds with no asymmetric gatekeeping between men and women, and no religious-authority capture of legitimacy. Suppression is low (0.22) because enforcement of gender-symmetric contract law faces normalization resistance (especially from religious and patriarchal traditions), but not from the individuals the constraint claims to empower—the alternative (family or religious gatekeeping) is what those individuals are escaping from. Theater is minimal (0.12): the constraint does what it claims (registers contracts, enforces symmetric property rights, allows interfaith marriage), with little performative overlay. Accessibility collapse is low (0.31): alternatives remain available to those who reject state-registered marriage (religious ceremonies, customary marriage, cohabitation), but legal consequences (tax, custody, inheritance) accrue only to state-registered status. The measurement trajectory shows stability: extractiveness and suppression both decline slightly in the early interval (normalization effect as the reading becomes established), then stabilize. This differs from the siblings: a religious-authority reading would show rising theater as enforcement machinery adapts to challenges, and potentially rising extractiveness as the authority locks in gatekeeping. The secular reading's stability is the diagnostic mark of genuine coordination without active rent-seeking.
 *
 * PERSPECTIVAL GAP:
 *   The state registration authority and the women/interfaith seats should compute to different types from this story. From the state's seat, the constraint is coordination it provides and maintains (rope or scaffold); from the women's seat, it is rope-or-better (they exit patriarchal extraction). From a religious-authority seat (excluded from this reading but present as a sibling seat), the constraint would compute as snare or tangled rope (they lose gatekeeping authority and their followers are coerced out of religious marriage into state registration). The engine's per-seat computation should reveal this divergence. The authored story claims all three seats sit on the cooperative side of the spectrum, but the religious-authority sibling will claim they sit on the extractive side. That divergence—same kernel constraint, different readings, different per-seat types—is exactly what the kernel-reading machinery is meant to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   The state registration authority and the beneficiary seats (women, interfaith couples, men under this reading) should compute differently from this story's authored perspective: the state sits at moderately extractive directionality (d~0.55-0.65) because it collects fees and enforces registration as a bottleneck, but the beneficiary seats sit near beneficiary directionality (d~0.2-0.3) because they receive autonomy and legal capacity gains from the same structure. The low suppression (0.22) and low resistance (0.58 as authored, below the midpoint) reflect the reading's claim that the constraint operates with willing participation—individuals benefit from getting out of patriarchal or religious gatekeeping, and the state is providing a coordination service they choose to use. Contrast this with a religious-authority reading where resistance would be higher (from individuals rejecting that authority) and suppression would be higher (that authority would need to defend against competing legitimacy claims). The secular reading's directionality derivation flows from: beneficiaries (women, interfaith) have trapped→mobile exit shift (the constraint enables exit from previous patriarchal constraints), so they derive low d; the state has collected benefits (registration fees, tax compliance, unified law) so it derives higher d; but the difference is smaller than in extractive readings because the state frames itself as a neutral coordinator, not as extracting rents. This structural claim is what the metrics assess.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (religious fragmentation + patriarchal disability) was live at the reading's inception and remains contested. The state-registration constraint claims to solve it by removing religious and family gatekeeping. Religious authorities dispute that the problem is real (patriarchy is doctrine, not disability) and dispute that the secular solution is legitimate. The mandatrophy question: has the founding problem been solved such that the state-registration constraint now persists without function? The measurement trajectory shows stable extractiveness and suppression, not the rising theater that signals functionally dead constraint persisting theatrically. This suggests the founding problem remains live (women still face religious and family barriers to marriage autonomy in many contexts, interfaith couples still face religious gatekeeping). However, in jurisdictions where secular civil law has become culturally dominant and religious marriage is optional ceremony rather than legal necessity, there is a secondary mandatrophy risk: the constraint's coordination function could atrophy while its extraction (registration fees, tax enforcement, state control) persists. The trajectory does not yet show this (theater ratio is stable, not rising), but the omega around authority-competition dynamics documents this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authority_competition_dynamics,
    'As state-registered marriage becomes culturally dominant, does the constraint''s coordination function remain live or has it atrophied into pure regulatory capture?',
    'Comparative analysis across jurisdictions: (1) where religious marriage retains social legitimacy alongside state registration, the coordination function persists; (2) where state registration is obligatory cultural norm and religious ceremony is purely ceremonial, the coordination function has atrophied and extraction (fees, tax enforcement, state gatekeeping) persists as pure function.',
    'If atrophied, the constraint should reclassify from rope to tangled_rope or snare in high-secularization jurisdictions, while remaining rope in pluralistic contexts. This means the kernel reading''s type varies by jurisdiction and historical moment—a reading-seat-time-index computation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_competition_dynamics, empirical, 'Whether the secular reading''s coordination function has outlived the founding problem or persists as regulatory capture.').

omega_variable(
    gender_symmetry_implementation,
    'Does this reading''s formal gender symmetry in contract law translate into substantive equal capacity, or does patriarchal authority persist through informal enforcement and family pressure outside the legal frame?',
    'Empirical study of post-registration outcomes: (1) do women exit marriages at equal rates and recover equal property/custody shares; (2) do informal family pressures override formal contract symmetry; (3) are exit options genuinely mobile or do social costs of divorce create identity-lock despite legal capacity?',
    'If patriarchal authority persists informally, women''s exit_options should be recoded from mobile to identity_locked, directionality should shift from beneficiary (d~0.2) to near-symmetric (d~0.4-0.5), and the constraint should reclassify from rope toward tangled_rope or snare from the women''s seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_symmetry_implementation, empirical, 'Whether formal gender symmetry in contract law translates to substantive autonomy or masks persistent patriarchal enforcement.').

omega_variable(
    religious_authority_coexistence,
    'Does the secular reading coexist stably with religious-authority readings in pluralistic jurisdictions, or does state monopoly on legal validity structurally foreclose religious gatekeeping?',
    'Historical and ethnographic analysis of religious communities'' adaptation: (1) do they accept state registration as administrative requirement while maintaining religious marriage as primary legitimacy source; (2) do they contest state authority and resist registration; (3) does one authority eventually displace the other?',
    'If coexistence is unstable and state authority forecloses religious authority over time, the reading_relations should shift from coexists_with to forecloses. If coexistence is stable across centuries, the relation holds as coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_authority_coexistence, empirical, 'Whether secular state authority forecloses or coexists with religious marriage authority over the long term.').

omega_variable(
    contractual_framing_boundary,
    'Can marriage plausibly be framed as a civil contract between autonomous individuals, or does marriage''s social function (kinship constitution, child-rearing coordination, property succession) necessarily implicate family and community, making the contract framing partial or misleading?',
    'Jurisprudential and anthropological analysis: do contract-law frameworks capture all the binding dimensions of marriage, or do kinship obligations, community recognition, and child-welfare considerations exceed the contract model? Are these dimensions formally codified in family law or left to informal enforcement?',
    'If marriage necessarily exceeds the contract frame, the reading''s foundational axiom (individual_autonomy_principle) requires qualification or faces internal contradiction. The constraint might reclassify from rope toward tangled_rope if contracts inadequately capture the actual binding mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contractual_framing_boundary, conceptual, 'Whether marriage can be adequately modeled as a civil contract or whether the model is fundamentally incomplete.').

omega_variable(
    interfaith_marriage_social_recognition,
    'Does legal permission for interfaith marriage (under this reading''s state law) translate into genuine social access, or do informal community sanctions create functional barriers despite legal permission?',
    'Empirical study of interfaith couples'' marriage rates, community response, and enforcement of informal sanctions. Compare legal permission with social recognition across religious communities.',
    'If informal sanctions severely restrict interfaith marriage despite legal permission, accessibility_collapse remains high (alternatives are functionally closed by community pressure) and suppression increases (community enforcement operates where law does not). The constraint might compute as snare from the interfaith couple''s seat, not rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interfaith_marriage_social_recognition, empirical, 'Whether legal interfaith marriage permission translates to substantive social access or is blocked by informal community enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__secular_contractual_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__secular_contractual_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(fami_tr_t5, family_law_authority__secular_contractual_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__secular_contractual_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(fami_tr_t15, family_law_authority__secular_contractual_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__secular_contractual_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(fami_tr_t25, family_law_authority__secular_contractual_reading, theater_ratio, 25, 0.11).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__secular_contractual_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__secular_contractual_reading, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__secular_contractual_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fami_be_t5, family_law_authority__secular_contractual_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(fami_be_t10, family_law_authority__secular_contractual_reading, base_extractiveness, 10, 0.39).
narrative_ontology:measurement(fami_be_t15, family_law_authority__secular_contractual_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(fami_be_t20, family_law_authority__secular_contractual_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(fami_be_t25, family_law_authority__secular_contractual_reading, base_extractiveness, 25, 0.37).
narrative_ontology:measurement(fami_be_t30, family_law_authority__secular_contractual_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(fami_be_t40, family_law_authority__secular_contractual_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__secular_contractual_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(fami_su_t5, family_law_authority__secular_contractual_reading, suppression_requirement, 5, 0.26).
narrative_ontology:measurement(fami_su_t10, family_law_authority__secular_contractual_reading, suppression_requirement, 10, 0.24).
narrative_ontology:measurement(fami_su_t15, family_law_authority__secular_contractual_reading, suppression_requirement, 15, 0.22).
narrative_ontology:measurement(fami_su_t20, family_law_authority__secular_contractual_reading, suppression_requirement, 20, 0.21).
narrative_ontology:measurement(fami_su_t25, family_law_authority__secular_contractual_reading, suppression_requirement, 25, 0.21).
narrative_ontology:measurement(fami_su_t30, family_law_authority__secular_contractual_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement(fami_su_t40, family_law_authority__secular_contractual_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__secular_contractual_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(family_law_authority__secular_contractual_reading, 0.12).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__parsi_zoroastrian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the contested kernel 'family_law_authority'. Each sibling reading (Hindu dharmaśāstra, Muslim sharīʿah, Christian canonical, Parsi Zoroastrian) instantiates a different authority structure grounding marriage validity. They differ in: (1) ε value (extraction level grounded in that reading's authority structure); (2) beneficiary/victim structure (who gains legal capacity under that reading's rules, who loses gatekeeping authority); (3) gender-symmetry assumptions (this secular reading assumes gender-neutral contract; religious readings typically encode gender-differentiated rights); (4) interfaith-marriage permissibility (this reading permits; dharmaśāstra and Zoroastrian readings typically prohibit; sharīʿah permits with conditions). The readings are linked by network.affects_constraints because they compete for institutional authority over the same kernel—where one reading's authority is established, it structures the others' legitimacy space. The secular reading claims to bypass religious authority entirely, which forecloses religious readings' claims to absolute gatekeeping authority, but the religious readings coexist where communities maintain parallel marriage systems (state registration + religious ceremony). The network captures this kernel-level dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
