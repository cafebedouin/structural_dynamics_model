% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__republican_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Republican Reading of Sovereign Legitimacy (Popular Consent / Social Contract)
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   This story instantiates the republican reading of the
 *   sovereign_legitimacy kernel: authority is legitimate because and only
 *   because it is delegated upward from the people through consent mechanisms
 *   (elections, constitutional ratification, social contract theory). The
 *   reading's own history shows a franchise that began narrow
 *   (property-owning men) and expanded under sustained pressure (abolition,
 *   suffrage movements, civil rights enforcement) — the
 *   extraction/suppression decline in the measurement series traces that
 *   expansion, not a change in the underlying theory. The reading remains
 *   structurally tangled: it genuinely solves a coordination problem (how to
 *   exercise coercive power without force or inherited status) while
 *   simultaneously producing an asymmetric class of the excluded whose
 *   consent is claimed as the source of authority they never gave.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.42).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.38).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Republican Reading of Sovereign Legitimacy (Popular Consent / Social Contract)").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, '4913680b-fc8a-4868-b510-18a9f2c6b787').
narrative_ontology:cs_kernel_codification('4913680b-fc8a-4868-b510-18a9f2c6b787', formalized).
narrative_ontology:cs_authority_grounding('4913680b-fc8a-4868-b510-18a9f2c6b787', practice).
narrative_ontology:cs_interpretation_layer_present('4913680b-fc8a-4868-b510-18a9f2c6b787').
narrative_ontology:cs_reading_relation('4913680b-fc8a-4868-b510-18a9f2c6b787', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('4913680b-fc8a-4868-b510-18a9f2c6b787', sovereign_legitimacy__constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('4913680b-fc8a-4868-b510-18a9f2c6b787', foundational, authority_derives_from_governed_consent).
narrative_ontology:cs_axiom_status(authority_derives_from_governed_consent, holdable).
narrative_ontology:cs_axiom_grounding('4913680b-fc8a-4868-b510-18a9f2c6b787', authority_derives_from_governed_consent, conventional).
narrative_ontology:cs_axiom('4913680b-fc8a-4868-b510-18a9f2c6b787', foundational, periodic_electoral_revalidation_required_for_legitimacy).
narrative_ontology:cs_axiom_status(periodic_electoral_revalidation_required_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('4913680b-fc8a-4868-b510-18a9f2c6b787', periodic_electoral_revalidation_required_for_legitimacy, instrumental).
narrative_ontology:cs_axiom('4913680b-fc8a-4868-b510-18a9f2c6b787', secondary, inherited_bloodline_confers_no_authority).
narrative_ontology:cs_axiom_status(inherited_bloodline_confers_no_authority, holdable).
narrative_ontology:cs_axiom_grounding('4913680b-fc8a-4868-b510-18a9f2c6b787', inherited_bloodline_confers_no_authority, deontological).
narrative_ontology:cs_reference_frame('4913680b-fc8a-4868-b510-18a9f2c6b787', popular_sovereignty_social_contract).
narrative_ontology:cs_drift_state('4913680b-fc8a-4868-b510-18a9f2c6b787', contemporary_democratic_backsliding_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4913680b-fc8a-4868-b510-18a9f2c6b787', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, enfranchised_citizenry).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, elected_officeholders).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, disenfranchised_residents).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, non_citizen_denizens).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, structurally_underrepresented_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, structurally_underrepresented_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the vote and the formal claim that authority derives from their consent. Periodically ratifies or removes officeholders through elections. Benefits from the legitimating story (their will is sovereign) but also bears the diffuse costs of majoritarian outcomes they did not individually choose; exit means emigration or civil disobedience, both costly.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, enfranchised_citizenry, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, enfranchised_citizenry, agenda_setter).

% Derive authority to govern from periodic electoral validation. Administer the consent mechanism (elections, constitutional procedure) that both empowers and constrains them. Face removal at the next cycle, which disciplines behavior but also incentivizes short-horizon governance and majoritarian pandering.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, elected_officeholders, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, elected_officeholders, beneficiary).

% Live under laws and enforcement they cannot vote to change — historically the young, formerly enslaved or colonized populations, felons, and others excluded by franchise rules set by the enfranchised majority. Bear the coercive force of a government whose legitimacy claim (consent of the governed) does not extend to them by its own operative definition of 'the people.'
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, disenfranchised_residents, payer,
    powerless, biographical, trapped, national).

% Resident, taxed, and subject to the state's laws and enforcement apparatus, but categorically excluded from the consent mechanism (the franchise) that the reading treats as the source of legitimate authority. Their only leverage is advocacy through enfranchised intermediaries or eventual naturalization.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, non_citizen_denizens, payer,
    powerless, biographical, constrained, national).

% Formally enfranchised but structurally outvoted by durable majorities or districting/apportionment arrangements that dilute their electoral weight. Nominally beneficiaries of the consent story but experience majoritarian outcomes as persistent extraction with no realistic removal mechanism available to them alone; the tyranny-of-the-majority failure mode the reading's own theorists flag.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, structurally_underrepresented_minorities, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, structurally_underrepresented_minorities, beneficiary).

% Interpret whether the consent mechanism and its constitutional constraints have been honored, and can check majoritarian action that violates the boundaries of the social contract as codified. Neither directly elected nor directly extractive, but their rulings determine whether the legitimacy chain from people to officeholder holds.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__republican_reading, constitutional_courts, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__republican_reading, diffuse).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how coercive state power can be exercised over free and formally equal individuals without recourse to force alone or inherited status — by tying the right to govern to periodic, revocable authorization from the governed, coordination is achieved without requiring unanimous agreement on every decision.
% TRANSFER_FUNCTION: Moves the right to exercise coercive authority from the diffuse citizenry to a temporary set of officeholders, in exchange for periodic accountability (elections) and adherence to constitutional limits; simultaneously moves the costs of majority decisions onto whoever falls outside the winning coalition, including those never permitted into the coalition at all.
% ABSENT_VOICES: Disenfranchised residents, non-citizen denizens, and future generations bound by present constitutional commitments have no seat in the consent mechanism that is claimed to legitimate authority over them; historically this included women, enslaved and colonized populations, and property-less men, whose absence the reading's own later self-corrections (suffrage expansions) implicitly concede as a defect.
% DISAPPEARANCE_RATIONALE: If the electoral consent mechanism vanished overnight, officeholders would lose their claim to authority and successors would have to ground governance in force, inheritance, or some alternative legitimation story — the entire apparatus of removal, campaigning, constitutional adjudication, and peaceful transfer of power would need replacement.
% FOUNDING_PROBLEM: How to ground the legitimate exercise of coercive state power after rejecting inherited/divine-right monarchy, in a context (18th-century Atlantic revolutions) where neither force nor tradition alone could command obedience from populations that had explicitly repudiated those grounds.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative political scientists outside the citizenry/officeholder beneficiary set attest the founding problem (grounding authority without monarchy) was genuinely solved for enfranchised populations, but note independently — via suffrage-expansion historiography and critical race/gender scholarship — that the 'people' whose consent grounds authority was drawn narrowly at founding and has required repeated, contested expansion; this is not merely the reading's own self-report.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__republican_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).
:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because officeholder authority is genuinely time-limited and removable, unlike the monarchical reading's unconditional grant — but it is not zero, because durable majorities, districting, and franchise boundaries let the 'consent of the governed' claim launder outcomes imposed on non-consenting or under-weighted minorities. Suppression fell over the interval (0.6 to 0.38) as historical franchise restrictions were repealed, but remains non-trivial: the state still enforces laws on residents excluded from the mechanism claimed to legitimate those laws. Theater rose modestly (0.2 to 0.28) as symbolic electoral ritual (turnout drives, civic ceremony) has grown relative to the marginal decisiveness of any given vote in entrenched two-party or single-party-dominant systems.
 *
 * DIRECTIONALITY LOGIC:
 *   Enfranchised citizens and elected officeholders sit near the beneficiary end: they hold the formal consent right and the formal authority it grants, respectively, even though officeholders also bear accountability costs. Disenfranchised residents and non-citizen denizens sit near the full-target end: they bear the coercive force of laws passed under a legitimacy claim that structurally excludes their consent, with limited-to-no exit. Structurally underrepresented minorities are the reading's own diagnosed failure mode — nominal beneficiaries (they vote) who function much closer to payers because durable majority coalitions can outvote them indefinitely without any single election correcting the imbalance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (grounding coercive authority without force or inherited right) is genuinely solved for the enfranchised — this prevents mislabeling the whole arrangement as pure extraction. But the reading has repeatedly required its own boundary (who counts as 'the people') to be forcibly renegotiated from outside the beneficiary class, which is exactly the mismatch the R5 corroboration surfaces: status is properly read as contested rather than settled, and the disappearance_verdict of world_rearranges confirms this is not inertial residue — real arrangements (courts, elections, transfers of power) depend on it continuing to function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    franchise_boundary_as_kernel_or_drift,
    'Is the historically narrow franchise (property-owning men at founding) a drift away from the republican reading''s own foundational premise, or was the premise always compatible with a restricted ''people''?',
    'Textual and historical analysis of founding social-contract theorists'' own stated scope conditions for ''the people,'' cross-checked against subsequent suffrage-expansion arguments made from within the same tradition versus arguments requiring outside pressure to prevail.',
    'If the restricted franchise was always compatible with the premise, the reading''s core axiom (authority from popular consent) was never violated by exclusion — only its application scope changed, which weakens the disenfranchisement-as-victim framing. If the premise logically implies universal franchise and was merely violated in practice, the exclusion is a genuine internal contradiction the reading has had to repeatedly correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(franchise_boundary_as_kernel_or_drift, conceptual, 'Whether historical franchise restriction is compatible with or contradicts the republican reading''s own foundational premise.').

omega_variable(
    majoritarian_tyranny_severity,
    'Does the electoral removal mechanism sufficiently discipline majoritarian extraction against structurally underrepresented minorities, or does it structurally fail to protect them regardless of electoral cycle length?',
    'Comparative analysis of minority-protective outcomes across republican systems with varying levels of constitutional constraint (judicial review, supermajority requirements, entrenched rights) versus pure majoritarian systems.',
    'If constitutional constraints reliably correct majoritarian extraction, the tangled_rope classification is closer to the rope end for this population; if they do not, the underrepresented-minority payer role is closer to the full extraction end regardless of formal enfranchisement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_tyranny_severity, empirical, 'Whether constitutional constraint mechanisms adequately correct majoritarian tyranny against minorities.').

omega_variable(
    sibling_reading_framing_choice,
    'Could this constraint have been framed instead as a variant of the constitutional_hybrid_reading, given that most actually-existing republics retain some inherited or ceremonial residue (e.g., constitutional founding documents treated with quasi-sacred authority)?',
    'Case-by-case review of whether the specific republic modeled has any inherited/ceremonial authority component; a purely republican system with zero inherited residue (no monarchy, no sacralized founding document) versus one with residual ceremonial elements would sort differently.',
    'If most real republics retain ceremonial residue, this story models an idealized pure-republican case rather than any single actually-existing state; a hybrid framing would shift some structural weight to constitutional courts as co-authority rather than pure interpreters.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_framing_choice, conceptual, 'Alternative framing consideration: whether this reading models a pure case or should be merged with the hybrid reading for most real-world instances.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__republican_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(sove_tr_t0, observed).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__republican_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(sove_tr_t40, observed).
narrative_ontology:measurement(sove_tr_t80, sovereign_legitimacy__republican_reading, theater_ratio, 80, 0.24).
narrative_ontology:measurement_basis(sove_tr_t80, observed).
narrative_ontology:measurement(sove_tr_t120, sovereign_legitimacy__republican_reading, theater_ratio, 120, 0.25).
narrative_ontology:measurement_basis(sove_tr_t120, observed).
narrative_ontology:measurement(sove_tr_t180, sovereign_legitimacy__republican_reading, theater_ratio, 180, 0.27).
narrative_ontology:measurement_basis(sove_tr_t180, observed).
narrative_ontology:measurement(sove_tr_t240, sovereign_legitimacy__republican_reading, theater_ratio, 240, 0.28).
narrative_ontology:measurement_basis(sove_tr_t240, observed).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__republican_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(sove_be_t0, observed).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__republican_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement_basis(sove_be_t40, observed).
narrative_ontology:measurement(sove_be_t80, sovereign_legitimacy__republican_reading, base_extractiveness, 80, 0.47).
narrative_ontology:measurement_basis(sove_be_t80, observed).
narrative_ontology:measurement(sove_be_t120, sovereign_legitimacy__republican_reading, base_extractiveness, 120, 0.44).
narrative_ontology:measurement_basis(sove_be_t120, observed).
narrative_ontology:measurement(sove_be_t180, sovereign_legitimacy__republican_reading, base_extractiveness, 180, 0.43).
narrative_ontology:measurement_basis(sove_be_t180, observed).
narrative_ontology:measurement(sove_be_t240, sovereign_legitimacy__republican_reading, base_extractiveness, 240, 0.42).
narrative_ontology:measurement_basis(sove_be_t240, observed).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__republican_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement_basis(sove_su_t0, observed).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__republican_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(sove_su_t40, observed).
narrative_ontology:measurement(sove_su_t80, sovereign_legitimacy__republican_reading, suppression_requirement, 80, 0.46).
narrative_ontology:measurement_basis(sove_su_t80, observed).
narrative_ontology:measurement(sove_su_t120, sovereign_legitimacy__republican_reading, suppression_requirement, 120, 0.42).
narrative_ontology:measurement_basis(sove_su_t120, observed).
narrative_ontology:measurement(sove_su_t180, sovereign_legitimacy__republican_reading, suppression_requirement, 180, 0.39).
narrative_ontology:measurement_basis(sove_su_t180, observed).
narrative_ontology:measurement(sove_su_t240, sovereign_legitimacy__republican_reading, suppression_requirement, 240, 0.38).
narrative_ontology:measurement_basis(sove_su_t240, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__republican_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__republican_reading, 0.12).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the sovereign_legitimacy kernel. monarchical_reading grounds authority downward from inherited/divine right (expected low accountability, high suppression, low ongoing validation cost). constitutional_hybrid_reading splits authority between inherited ceremonial and delegated political sources. This republican_reading is distinguished by ONGOING validation cost (electoral cycles) as the mechanism binding authority to consent, producing moderate ε with a distinct victim class (those excluded from the franchise) rather than the monarchical reading's subject-class or the hybrid's mixed class. Each reading is authored as its own ε-invariant constraint; do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
