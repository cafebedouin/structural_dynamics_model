% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__anti_caste_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__anti_caste_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: fourteenth_amendment_equal_protection__anti_caste_reading
 *   human_readable: Fourteenth Amendment Equal Protection — Anti-Caste Reading
 *   domain: constitutional/civil_rights/political_philosophy
 *
 * SUMMARY:
 *   The Fourteenth Amendment's Equal Protection Clause is a contested
 *   constitutional kernel. This constraint instantiates the anti-caste
 *   reading: Equal Protection requires active state dismantling of racial,
 *   gender, and status hierarchies through corrective programs, wealth
 *   redistribution, institutional reform, and legal recognition of
 *   subordination as a state harm. The sibling formal-equality reading reads
 *   the same Clause as prohibiting explicit state racial classification and
 *   mandating race-neutrality. These are not empirical disagreements about
 *   what the Constitution says—they are structural disagreements about what
 *   commitment the Clause makes. The anti-caste reading legitimates
 *   affirmative state action and treats subordinated groups as beneficiaries
 *   of a constitutional mandate. The formal-equality reading treats
 *   race-consciousness as the violation. They occupy the same constitutional
 *   text and cannot both be the law of the land in a single jurisdiction at
 *   the same time; yet both remain live readings held by different judicial
 *   coalitions and legal scholars. The claim/metric gap is deliberate and
 *   structural: the anti-caste reading claims tangled rope (genuine
 *   coordination problem + active enforcement + beneficiary/victim asymmetry)
 *   while the authored metrics describe substantial extractiveness and
 *   suppression because the constraint's operation over its interval will
 *   require active dismantling of established privilege, resistance from
 *   those losing unearned advantage, and institutional friction of
 *   remediation.
 *
 * KEY AGENTS:
 *   - Subordinated racial and gender groups: primary beneficiaries under this reading; structurally subordinated; identity-locked or constrained exit
 *   - Dominant racial groups: bearing the cost of privilege loss; mobile exit options but constrained by national scope
 *   - State administrators and courts: agenda-setters; constrained by constitutional mandate to implement; bearing operational costs
 *   - Taxpayers funding corrective programs: payers; moderate power; constrained exit via taxation
 *   - Formal-equality advocates: excluded from this reading's frame; institutional power but no voice in anti-caste agendas
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.68).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.71).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "Fourteenth Amendment Equal Protection — Anti-Caste Reading").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional/civil_rights/political_philosophy").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, '1b296334-7e86-4e2a-9fdb-230537218635').
narrative_ontology:cs_kernel_codification('1b296334-7e86-4e2a-9fdb-230537218635', fixed_text).
narrative_ontology:cs_authority_grounding('1b296334-7e86-4e2a-9fdb-230537218635', lineage).
narrative_ontology:cs_interpretation_layer_present('1b296334-7e86-4e2a-9fdb-230537218635').
narrative_ontology:cs_reading_relation('1b296334-7e86-4e2a-9fdb-230537218635', fourteenth_amendment_equal_protection__formal_equality_reading, coexists_with).
narrative_ontology:cs_axiom('1b296334-7e86-4e2a-9fdb-230537218635', foundational, structural_subordination_is_state_harm).
narrative_ontology:cs_axiom_status(structural_subordination_is_state_harm, holdable).
narrative_ontology:cs_axiom_grounding('1b296334-7e86-4e2a-9fdb-230537218635', structural_subordination_is_state_harm, deontological).
narrative_ontology:cs_axiom('1b296334-7e86-4e2a-9fdb-230537218635', foundational, equal_dignity_requires_affirmative_remedy).
narrative_ontology:cs_axiom_status(equal_dignity_requires_affirmative_remedy, holdable).
narrative_ontology:cs_axiom_grounding('1b296334-7e86-4e2a-9fdb-230537218635', equal_dignity_requires_affirmative_remedy, deontological).
narrative_ontology:cs_reference_frame('1b296334-7e86-4e2a-9fdb-230537218635', reconstruction_mandate_to_dismantle_caste).
narrative_ontology:cs_drift_state('1b296334-7e86-4e2a-9fdb-230537218635', contemporary_backlash_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1b296334-7e86-4e2a-9fdb-230537218635', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_racial_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, women_and_gender_minorities).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, economically_subordinated_classes).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, dominant_racial_groups_losing_unearned_privilege).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, state_administrators_bearing_compliance_cost).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, taxpayers_funding_corrective_programs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, affected_descendants_of_historical_subordination).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, state_administrators).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, structural_subordination_is_state_harm).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, equal_dignity_requires_affirmative_remedy).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, hierarchy_persistence_implicates_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of racial groups historically subjected to state-enforced subordination and ongoing structural inequality (Black Americans, Indigenous peoples, other racialized minorities). Under this reading, they are the primary beneficiaries of Equal Protection because the constraint obligates the state to actively dismantle the hierarchies that harm them. Their exit is constrained: citizenship cannot be abandoned; relocation within the nation does not escape the subordinating structure; international migration is available only for those with resources.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_racial_groups, beneficiary,
    organized, generational, constrained, national).

% Persons subjected to state-enforced gender hierarchy and discrimination. Under this reading, Equal Protection requires active dismantling of gender-based subordination through corrective state action (affirmative measures, remedial programs, structural reform). Exit is similarly constrained: gender subordination operates across all jurisdictions; leaving the nation is not a realistic remedy for internal subordination.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, women_and_gender_minorities, beneficiary,
    organized, generational, constrained, national).

% Low-income and economically precarious populations whose subordination through wealth concentration and access restriction is treated, under this reading, as a state concern implicating Equal Protection. They benefit from programs and structural changes that the anti-caste reading legitimates as remedial state action. Exit is constrained: economic circumstance is systemically reproduced; capital mobility is not available to those without resources.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, economically_subordinated_classes, beneficiary,
    moderate, generational, constrained, national).

% Members of racial groups that have benefited from state-enforced racial hierarchy (white Americans in the US context). Under this reading, they bear a cost: the loss of unearned privilege that the constraint's operation dismantles, and the necessity of living in a society structured for equal dignity rather than racial dominance. Exit options include geographic relocation, capital flight, or private institution-building that replicates hierarchical structure outside state purview.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, dominant_racial_groups_losing_unearned_privilege, payer,
    powerful, biographical, mobile, national).

% Government officials at federal, state, and local levels bear the operational cost of active dismantling: designing remedial programs, monitoring compliance, defending programs against legal challenge, and managing the institutional friction of redistribution. They set agenda as the constraint's implementers, but are constrained by the constitutional mandate itself—they cannot simply avoid it without constitutional amendment or jurisdictional exit. They also bear the cost of managing resistance and political backlash.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, state_administrators, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__anti_caste_reading, state_administrators, payer).

% Citizens whose tax revenue funds the affirmative state action required under this reading (affirmative action programs, wealth redistribution, reparative investment, dismantling infrastructure). They bear the direct cost of remedial expenditure. Exit via tax avoidance or relocation carries legal and practical constraints; capital flight is available only to high-net-worth individuals.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, taxpayers_funding_corrective_programs, payer,
    moderate, biographical, constrained, national).

% Legal scholars, judges, and policymakers who hold the sibling formal-equality reading and view the anti-caste reading as over-reading the Clause. They are excluded from the anti-caste reading's framing but hold significant institutional power in constitutional interpretation. They would argue that Equal Protection prohibits race-consciousness, not mandates it, and that the anti-caste reading invents a remedial duty not present in the text or founding intent. Their presence in appellate courts and influential academic positions means they suppress or reverse anti-caste rulings.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, formal_equality_advocates, excluded,
    institutional, generational, analytical, national).

% Persons whose identity as members of subordinated groups is constitutionally salient under this reading—they cannot shed that identity without abandoning their actual social positioning and historical connection to subordinated lineages. The constraint benefits them through recognition that their subordination is a state problem requiring active remedy, but they cannot exit subordination by choice; it is structurally imposed. Identity-locked exit reflects that their position in the hierarchy is not volitional and that accepting the benefit of the constraint requires accepting identity as a subordinated-group member.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, affected_descendants_of_historical_subordination, beneficiary,
    moderate, generational, identity_locked, national).

% The federal courts, especially the Supreme Court, set the agenda by interpreting what Equal Protection requires. Under this reading, they must affirmatively mandate state dismantling of hierarchies. Under the sibling reading, they must police race-consciousness. Their exit is constrained by the constitutional text and binding precedent; they can reinterpret the Clause, but cannot avoid the question. Composition shifts change which reading gains institutional power.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, supreme_court_and_federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_racial_groups).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__anti_caste_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The anti-caste reading coordinates a national commitment that structural subordination is a state harm requiring active remedy. It solves the collective-action problem of moving from passive non-discrimination to affirmative reconstruction: without a binding constitutional interpretation, states and individuals lack coordination on whether equal dignity demands corrective action or only forbearance.
% TRANSFER_FUNCTION: Transfers unearned privilege and the costs of hierarchy maintenance FROM dominant groups TO subordinated groups, via state-mandated corrective programs, wealth redistribution, institutional reform, and changed legal recognition. The transfer is legitimated as rectifying unjust hierarchy, not as zero-sum redistribution.
% ABSENT_VOICES: Formal-equality advocates and originalist scholars are structurally excluded from the anti-caste reading's interpretive frame—they would argue the Clause does not mandate active dismantling and that race-consciousness in remedy violates the color-blind ideal. Their absence from agenda-setting in jurisdictions that adopt the anti-caste reading means the constitutional reading is not debated in the open; it is imposed as law. Descendants of enslaved and colonized peoples whose forced labor and expropriation created wealth that persists in dominant groups have historically been excluded from reparative conversations entirely.
% DISAPPEARANCE_RATIONALE: If the anti-caste reading of Equal Protection disappeared and only formal-equality reading governed, the state would cease affirmative dismantling of hierarchy—affirmative action programs, diversity remedies, reparative investment, and structural reforms justified on anti-caste grounds would end or face severe legal constraint. Institutional hierarchies would stabilize along racial and gender lines. Wealth concentration in dominant groups would persist. The world would rearrange toward entrenchment of existing subordination patterns.
% FOUNDING_PROBLEM: The Fourteenth Amendment was drafted to address slavery and caste-like subordination of freed slaves and their descendants, and the subordination of women and other marginalized groups locked into legal hierarchies. The founding problem was: how to construct a nation where subordination is not institutionalized and where equal dignity is guaranteed—not merely passive non-discrimination but active state obligation to dismantle caste structures.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights scholars and reparations advocates (outside the beneficiary set) cite historical record and ongoing structural inequality as evidence the founding problem is live. Formal-equality advocates and originalists contest that the founding problem was ever framed as requiring affirmative dismantling; they argue the Clause was meant to forbid explicit hierarchy, not mandate rectification. No contemporary source outside the founding era attests the original understanding unambiguously—the dispute is located in contested historical interpretation itself.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__anti_caste_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__anti_caste_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__anti_caste_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the constraint requires sustained transfer of resources and privilege from dominant to subordinated groups—this is not a one-time coordination, but active, ongoing dismantling. Suppression is high (0.71) because resistance from those losing privilege must be actively managed; the constraint persists against substantial countervailing force. Theater ratio is moderate (0.42) because some remedial programs are performative (diversity theater without structural change) while others are functionally directed at hierarchy dismantling—the ratio reflects the mixture. Accessibility collapse is low (0.48) because alternatives to the anti-caste reading remain live in legal discourse; the formal-equality reading is equally coherent and occupies the same constitutional text. Resistance is high (0.72) because the constraint meets fierce opposition from those who benefit from hierarchy maintenance and those who genuinely believe the formal-equality reading is correct. The measurement series models the projection that if the anti-caste reading gains institutional traction, extractiveness will rise over time as remedial programs deepen and resistance will continue at high levels.
 *
 * PERSPECTIVAL GAP:
 *   The formal-equality advocate and the anti-caste beneficiary compute different types from the same constitutional text because they occupy different structural positions relative to the hierarchy being dismantled. From the formal-equality seat, Equal Protection forbids race-consciousness and the state must be neutral—active dismantling violates this reading and looks like extraction from the dominant group being displaced. From the beneficiary seat, Equal Protection requires active dismantling because subordination itself is the violation—passivity maintains harm. The engine computes this divergence from the structural data (beneficiary/victim declarations, power atoms, exit options) independently of the claim. The two seats should produce different type verdicts for the same constraint because their directionalities are opposite (beneficiary near d=0, payer near d=1).
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinated groups occupy the beneficiary position (d toward 0): the constraint legitimates their claims on state action for dismantling hierarchy; they benefit from interpretation that treats their subordination as a state harm. Dominant groups and taxpayers occupy the payer position (d toward 1): they bear the cost of privilege loss and remedial expenditure. State administrators occupy an agenda-setter position (d moderate) because they must implement the constraint but are constrained by it—they cannot simply avoid it. The formal-equality advocates are excluded (they would oppose this reading) but have institutional power that suppresses its full implementation. Directionality derives from who benefits, who pays, and the structure of exit—subordinated groups cannot escape subordination by relocation; dominant groups can migrate capital or institutions but cannot avoid national citizenship.
 *
 * MANDATROPHY ANALYSIS:
 *   The anti-caste reading avoids mandatrophy (founding problem now dead, constraint persists as theater) because the founding problem—structural subordination—remains live as long as hierarchies persist. Contestation over whether the problem is live or solved is itself the evidence of continued dispute. The constraint does not persist by inertia; it persists because subordinated groups continue to claim it, and because state institutions (courts, legislatures) are actively interpreting and implementing it. However, the theater ratio reflects a real danger: some programs labeled affirmative action or diversity remedies may be performative without structural change, using the language of anti-caste dismantling to maintain appearance of progress while hierarchy persists. The measurement trajectory (theater ratio rising slightly over time) models a concerning scenario where the reading gains legitimacy but actual institutional change lags—the constraint becomes theater. An omega variable captures this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    affirmative_remedy_vs_neutral_state,
    'Does the Fourteenth Amendment''s text commit to active dismantling of hierarchies, or only to neutral non-discrimination?',
    'Historical evidence from Reconstruction debates, legislative record of the Clause''s sponsors, and canonical interpretations from contemporary courts. The resolution cannot be purely historical because the text is interpreted in light of current constitutional law; instead, resolution lies in sustained precedent from appellate courts settling which reading is ''the law.''',
    'If active dismantling: the anti-caste reading is correct and the constraint''s extraction is justified as remedy. If neutral state: the formal-equality reading is correct and the constraint''s extraction (forced privilege loss) is a violation of equal protection itself. The constraint''s type depends on which reading wins institutional adoption.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(affirmative_remedy_vs_neutral_state, conceptual, 'Whether the Fourteenth Amendment commits to affirmative dismantling or neutral state.').

omega_variable(
    structural_subordination_as_state_harm,
    'Is ongoing structural subordination (wealth concentration, institutional exclusion, social devaluation) a direct state harm that Equal Protection obligates remedy for, or is it the cumulative effect of private choices and historical injury the state need not affirmatively fix?',
    'Causality evidence linking current state policy and institutional structure to ongoing subordination (redlining, police practices, school funding models, employment discrimination). Alternatively, accepted moral theory determining what harms the state is responsible for rectifying versus what are residual harms of historical injustice.',
    'If structural subordination is a state harm: the constraint''s extraction from dominant groups is justified remedy. If it is private injury: the state has no obligation to extract privilege and the constraint is overreach, not coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_subordination_as_state_harm, empirical, 'Whether ongoing structural inequality constitutes actionable state harm under Equal Protection.').

omega_variable(
    remedial_extraction_versus_tyranny_of_majority,
    'Can the anti-caste reading sustain itself against the claim that using state power to extract privilege from a majority (or powerful minority) is tyranny regardless of the justice of the goal?',
    'Political theory and constitutional doctrine on majority-rule limits, minority protection, and the permissible scope of collective action. Empirical evidence on whether remedial programs produce social stability or entrench counter-resistance.',
    'If remedial extraction is justified: the constraint''s suppression is legitimate. If it is tyranny: the constraint is illegitimate extraction disguised as justice and should be reclassified as snare. This is a preference omega—the resolution depends on accepting different frameworks of political legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedial_extraction_versus_tyranny_of_majority, preference, 'Whether active dismantling via state coercion is justified remedy or unjust tyranny.').

omega_variable(
    performance_versus_institutional_change,
    'As the anti-caste reading gains institutional legitimacy, will corrective programs translate into structural dismantling of hierarchy, or will they become performance theater—symbolic gestures of remediation without actual change in wealth, power, or institutional access?',
    'Measurement of actual outcomes: wealth gaps, representation in leadership, institutional access, and structural changes to systems producing subordination (policing reform, school funding equity, hiring practices). Compare program legitimacy to actual behavioral change.',
    'If programs produce structural change: the theater ratio remains moderate and the constraint sustains functional coordination. If programs become theater: the theater ratio rises, extraction persists (transferred resources flow to administrator structures, not to subordinated groups), and the constraint degrades toward piton or snare-with-capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_versus_institutional_change, empirical, 'Whether corrective programs will achieve institutional change or devolve into performative diversity theater.').

omega_variable(
    reading_stability_in_changed_composition,
    'If the judicial and political composition shifts such that formal-equality advocates gain institutional dominance, can the anti-caste reading survive as a living constraint, or does it require active judicial/legislative protection to persist?',
    'Observation of appellate court decisions overturning or limiting affirmative action and remedial programs; tracking whether anti-caste principles appear in statutory new law or only in reversible precedent.',
    'If the reading requires active protection: it is institutionally fragile and may collapse if political will shifts. If it becomes embedded in statute: it gains institutional resilience. This affects the temporal profile of the constraint and its long-term classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_stability_in_changed_composition, empirical, 'Whether the anti-caste reading has institutional durability or depends on current political alignment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t0, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(four_tr_t0, projected).
narrative_ontology:measurement(four_tr_t8, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(four_tr_t8, projected).
narrative_ontology:measurement(four_tr_t16, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(four_tr_t16, projected).
narrative_ontology:measurement(four_tr_t24, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(four_tr_t24, projected).
narrative_ontology:measurement(four_tr_t32, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(four_tr_t32, projected).
narrative_ontology:measurement(four_tr_t40, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(four_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(four_be_t0, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(four_be_t0, projected).
narrative_ontology:measurement(four_be_t8, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(four_be_t8, projected).
narrative_ontology:measurement(four_be_t16, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement_basis(four_be_t16, projected).
narrative_ontology:measurement(four_be_t24, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(four_be_t24, projected).
narrative_ontology:measurement(four_be_t32, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement_basis(four_be_t32, projected).
narrative_ontology:measurement(four_be_t40, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(four_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t0, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(four_su_t0, projected).
narrative_ontology:measurement(four_su_t8, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(four_su_t8, projected).
narrative_ontology:measurement(four_su_t16, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement_basis(four_su_t16, projected).
narrative_ontology:measurement(four_su_t24, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement_basis(four_su_t24, projected).
narrative_ontology:measurement(four_su_t32, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement_basis(four_su_t32, projected).
narrative_ontology:measurement(four_su_t40, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(four_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__anti_caste_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(fourteenth_amendment_equal_protection__anti_caste_reading, 0.14).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection__formal_equality_reading).

% DUAL FORMULATION NOTE:
% The Fourteenth Amendment Equal Protection Clause is the kernel. The anti-caste reading (this file) interprets it as requiring active dismantling of hierarchy; the formal_equality_reading interprets it as requiring race-neutrality and prohibiting race-consciousness. They are not compatible readings—they cannot both be law in a single jurisdiction at the same time—but they are both live positions in constitutional discourse. Both stories should be authored separately with independent ε, stakeholder maps, and metrics. The network edge links them as siblings of the same kernel dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fourteenth_amendment_equal_protection__anti_caste_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
