% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: Refugee Convention — Restrictive Sovereignty Reading
 *   domain: international_law/migration/human_rights
 *
 * SUMMARY:
 *   The 1951 Refugee Convention is a shared, written kernel text whose
 *   meaning is contested across state parties, UNHCR, and human-rights
 *   advocates. This constraint instantiates the RESTRICTIVE SOVEREIGNTY
 *   READING: the Convention as a minimum floor permitting maximum state
 *   discretion, requiring 'well-founded fear' to be proven individually and
 *   targeted, limiting 'particular social group' to immutable characteristics
 *   with documentary state awareness, and excluding generalized violence and
 *   non-state persecution. This reading operationalizes offshore processing,
 *   high admissibility screens, and narrow victim recognition. The reading is
 *   held as canonical by wealthy Global North states seeking to restrict
 *   flows and by some international law scholars emphasizing state
 *   sovereignty. It competes with an expansive humanitarian reading (broader
 *   persecution definition, gender/LGBTQ+/clan-based group recognition) and a
 *   procedural-integrity reading (process-centered, outcome-flexible). This
 *   story describes ONE reading; the expansive and procedural readings are
 *   separate constraint stories in the same family.
 *
 * KEY AGENTS:
 *   - Sovereign states (restrictive interpretation seat): set admissibility standards, enforce narrow definitions, claim the reading honors the Convention while preserving state discretion.
 *   - Asylum adjudication bodies: apply the reading in individual cases; bear legitimacy cost of exclusions while constrained by legal directives.
 *   - Excluded claimants (generalized violence, non-state persecution, gender-based, LGBTQ+, broad social groups): face closed doors despite danger; their persecution is redefined as non-qualifying.
 *   - Offshore processing operators and externalization-allied states: benefit from the reading's authorization of extraterritorial screening and reduced in-country admissions.
 *   - Expansive humanitarian jurisdictions and refugee advocates: contest the reading as a post-hoc narrowing that violates the Convention's humanitarian intent.
 *   - UNHCR and convention depositary: observe divergence and attempt guidance; their Handbook takes positions closer to the expansive reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.68).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.72).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "Refugee Convention — Restrictive Sovereignty Reading").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international_law/migration/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, '96ecb145-a10a-4205-88a4-0e866b2a7774').
narrative_ontology:cs_kernel_codification('96ecb145-a10a-4205-88a4-0e866b2a7774', fixed_text).
narrative_ontology:cs_authority_grounding('96ecb145-a10a-4205-88a4-0e866b2a7774', extraction).
narrative_ontology:cs_interpretation_layer_present('96ecb145-a10a-4205-88a4-0e866b2a7774').
narrative_ontology:cs_reading_relation('96ecb145-a10a-4205-88a4-0e866b2a7774', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('96ecb145-a10a-4205-88a4-0e866b2a7774', refugee_convention_text__procedural_integrity_reading, influences).
narrative_ontology:cs_axiom('96ecb145-a10a-4205-88a4-0e866b2a7774', foundational, individualized_persecution_requirement).
narrative_ontology:cs_axiom_status(individualized_persecution_requirement, holdable).
narrative_ontology:cs_axiom_grounding('96ecb145-a10a-4205-88a4-0e866b2a7774', individualized_persecution_requirement, conventional).
narrative_ontology:cs_axiom('96ecb145-a10a-4205-88a4-0e866b2a7774', foundational, state_directed_action_requirement).
narrative_ontology:cs_axiom_status(state_directed_action_requirement, holdable).
narrative_ontology:cs_axiom_grounding('96ecb145-a10a-4205-88a4-0e866b2a7774', state_directed_action_requirement, conventional).
narrative_ontology:cs_axiom('96ecb145-a10a-4205-88a4-0e866b2a7774', secondary, immutable_characteristic_gatekeeping).
narrative_ontology:cs_axiom_status(immutable_characteristic_gatekeeping, holdable).
narrative_ontology:cs_axiom_grounding('96ecb145-a10a-4205-88a4-0e866b2a7774', immutable_characteristic_gatekeeping, conventional).
narrative_ontology:cs_reference_frame('96ecb145-a10a-4205-88a4-0e866b2a7774', convention_individualized_protection_framework).
narrative_ontology:cs_drift_state('96ecb145-a10a-4205-88a4-0e866b2a7774', contemporary_mass_displacement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('96ecb145-a10a-4205-88a4-0e866b2a7774', '2026-06-11T09:15:32Z').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states_restrictive_interpretation).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, high_asylum_threshold_jurisdictions).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, generalized_violence_displaced).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, non_state_persecution_targets).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, gender_persecution_claimants).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, lgbtq_persecution_claimants).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, broad_social_group_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, offshore_processing_operators).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_adjudication_bodies).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, state_sovereignty_primacy).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, individualized_persecution_doctrine).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, immutable_characteristic_gatekeeping).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the 1951 Convention's language to permit maximum discretion in asylum admissions. Reads 'well-founded fear of persecution' as requiring individualized proof of state-directed harm, 'particular social group' as limited to immutable characteristics with demonstrable state awareness, and 'persecution' as excluding generalized violence and non-state harm. Sets screening thresholds, determines admissibility, and enforces the interpretation through immigration courts and administrative procedure. Claims the reading respects state sovereignty while fulfilling minimal Convention obligations.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states_restrictive_interpretation, agenda_setter,
    institutional, generational, arbitrage, national).

% Interpret the same Convention text to require broader protection: generalized violence as persecution; non-state harm as actionable; gender, sexual orientation, and clan-based group membership as qualifying 'particular social groups.' They adopt the restrictive reading as internally incoherent with the Convention's humanitarian purpose and argue for the expansive reading.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, expansive_humanitarian_jurisdictions, observer,
    institutional, generational, constrained, national).

% Immigration judges and administrative tribunals apply the restrictive reading in individual cases, using the narrow definitions of persecution and particular social group as gatekeeping standards. They bear the procedural load and legitimacy cost of denials; they cannot refuse to apply the reading without violating legal directives from the states that employ them.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_adjudication_bodies, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__restrictive_sovereignty_reading, asylum_adjudication_bodies, payer).

% Flee armed conflict, gang violence, or state collapse where harm is general and indiscriminate rather than targeted at them individually. Under the restrictive reading, they cannot demonstrate 'persecution' because no state or organized group specifically targeted them by identity; they fail the individualization gate and receive no protection despite facing objective danger.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, generalized_violence_displaced, payer,
    powerless, immediate, trapped, local).

% Face documented persecution by private criminal organizations, family members, or communal actors (honor-based violence, forced marriage, clan-based exile) rather than by states. The restrictive reading excludes non-state persecution as falling outside the Convention's scope because it limits 'persecution' to actions by or with state connivance; they lose protection despite meeting the humanitarian threshold.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, non_state_persecution_targets, payer,
    powerless, immediate, trapped, local).

% Flee systematic gender-based violence (forced marriage, domestic abuse, FGM, reproductive coercion) where the harm target is the gender itself, not a pre-existing immutable identity with state awareness. Under the restrictive reading, 'gender' does not qualify as a 'particular social group' because the group is constituted by the persecution itself, not by an antecedent immutable trait; they fall outside the protected category despite fleeing systematic harm.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, gender_persecution_claimants, payer,
    powerless, immediate, trapped, local).

% Face persecution for sexual orientation or gender identity in jurisdictions where the state does not formally criminalize these identities but enforcement and tacit state approval of mob violence is widespread. The restrictive reading requires documented state awareness and targeted action; tacit state tolerance or private violence does not meet the threshold; many LGBTQ+ claimants are excluded even in high-danger contexts.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, lgbtq_persecution_claimants, payer,
    powerless, immediate, trapped, local).

% Belong to socially defined groups (landless peasants, ethnic minorities without formal legal status, caste-based groups, political opposition networks) where group membership is socially meaningful and the target of persecution, but the group is constituted socially rather than by pre-existing immutable characteristic independently of the persecution claim. The restrictive reading rejects these as 'particular social groups' because they fail the immutability gate; they receive no protection despite systematic persecution.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, broad_social_group_claimants, payer,
    powerless, immediate, trapped, local).

% Human rights bodies, refugee advocates, and procedural-integrity-focused jurisdictions argue that the restrictive reading's gates (individualized proof, immutability requirement, state awareness) are substantive doctrine, not procedure, and distort the Convention's minimum protections. They see the reading as a deliberate narrowing that violates the Convention's principle of inclusive procedural assessment.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, procedural_integrity_advocates, observer,
    organized, generational, analytical, national).

% The UN administers the Convention, collects interpretations from state parties and UNHCR, and issues guidance (Handbook, General Comments) attempting to clarify the Convention's scope. The depositary observes that the restrictive reading is one interpretation held by some states, not the only permissible reading, and that divergence between states creates a fragmented protection landscape.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, convention_depositary, observer,
    institutional, generational, analytical, global).

% Third countries hosting regional processing centers (Australia's offshore regime, EU migration externalization) apply or are pressured to apply the restrictive reading to reduce admissions and shift costs. The restrictive reading operationally justifies offshore and extraterritorial processing by narrowing who qualifies for in-country review; they benefit from reduced flow and the authorization the reading provides for externalization.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, offshore_processing_operators, beneficiary,
    institutional, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__restrictive_sovereignty_reading, offshore_processing_operators).
narrative_ontology:fixing_cost_class(refugee_convention_text__restrictive_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Convention coordinate the asylum system by providing a shared legal framework for determining who qualifies for international protection and what obligations states bear. Under the restrictive reading, coordination is minimal: the Convention sets a low floor (individualized persecution only; immutable characteristics only; state action required), and states retain maximum discretion above that floor — they may be more generous, but the floor permits exclusion of entire classes of people in danger.
% TRANSFER_FUNCTION: Moves the burden of protection from wealthy northern-hemisphere states to transit countries, and from structured asylum systems to informal camps, irregular migration, and deportation-risk contexts. Wealth and security flow from those excluded by the restrictive reading toward states that maintain it and the private actors (offshore processors, detention operators) who profit from externalized enforcement.
% ABSENT_VOICES: Generalized violence displaced, non-state persecution targets, gender-persecution claimants, and LGBTQ+ claimants are structurally excluded from voice in asylum systems that apply the restrictive reading; they cannot testify to their persecution if it falls outside the narrow categories. Origin-country civil-society organizations documenting persecution in forms the restrictive reading excludes (clan violence, gender-based persecution, communal exile) are not seated in the interpretation process.
% DISAPPEARANCE_RATIONALE: If the restrictive reading disappeared — if states instead applied the expansive humanitarian reading — millions of currently excluded claimants would gain access to asylum systems, offshore processing would lose its primary enforcement justification, and the global migration geography would reorganize around broader protection obligations. States would face higher admissions, different resource flows, and pressure to address root causes rather than exclude categories of people. The reading's disappearance would fundamentally restructure migration governance.
% FOUNDING_PROBLEM: The 1951 Convention was drafted to address displacement from totalitarian persecution (Soviet bloc, Nazi genocide) by providing minimum protection standards. The restrictive reading claims to honor the original intent: persecution = state-directed targeted harm; the Convention protects from this, and states may add broader protections if they choose, but the floor is individualized state persecution.
% FOUNDING_PROBLEM_CORROBORATION: The restrictive-reading states (and some international law scholars aligned with sovereignty-first framings) attest the founding problem as live and their reading as faithful to original intent. The expansive-humanitarian advocates, UNHCR, human rights bodies, and refugee-hosting developing countries attest the founding problem as partially obsolete (modern displacement includes generalized violence, climate-driven exodus, gang violence) and the restrictive reading as a post-hoc narrowing that contradicts the Convention's humanitarian principle. The 1951 negotiating record contains ambiguity; the UNHCR Handbook (1979, updated 2019) explicitly endorses broader interpretation of 'persecution' and 'particular social group,' which contradicts the restrictive reading's claim to fidelity.
narrative_ontology:disappearance_verdict(refugee_convention_text__restrictive_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__restrictive_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__restrictive_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__restrictive_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__restrictive_sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects that the reading systematically excludes large classes of displaced people from protection, transferring burden to transit countries and displacement camps. The extraction is structural: the restrictive definitions are designed to limit admissions and justify non-admission of entire categories. Suppression (0.72) is high because the reading's enforcement requires active gatekeeping — immigration courts must reject claims that fall outside the narrow categories, and the categories themselves are jurisdictional doctrines, not natural facts. Theater (0.41) is moderate: the reading includes genuine procedural assessment and individualized review, but an increasing share of enforcement activity is devoted to justifying exclusions rather than assessing persecution. Accessibility collapse (0.58) is moderate because alternatives (expansive reading, humanitarian protection via domestic law) exist but are actively foreclosed by states applying the restrictive reading. Resistance (0.69) is substantial: human rights bodies, refugee advocates, developing countries, and UNHCR contest the reading. Temporal data: extractiveness plateaus at 0.68 as the reading stabilizes; theater rises as states invest more in justifying exclusions ('safe country of origin' presumptions, non-refoulement carve-outs); suppression stabilizes as enforcement machinery matures. The shared time grid allows all three metrics to be tracked coherently.
 *
 * PERSPECTIVAL GAP:
 *   The restrictive-reading seat views this constraint as a Rope: genuine coordination function (Convention provides shared standards and predictability) with minimal coercive overhead. The excluded-claimant seats view it as a Snare: the 'coordination function' is a cover story, and the actual structure is extraction and exclusion justified by narrow definitions designed for that purpose. The procedural-integrity seat views it as a Tangled Rope failing the procedural gate (the substantive narrowing violates the process-centered principle). The expansive-humanitarian seat views it as a false Mountain — a natural-law framing (persecution, immutable characteristics) that naturalizes constructed definitional choices. The engine computes per-seat classification from the structural data; divergence between seats reflects the genuine political contest over the Convention's meaning.
 *
 * DIRECTIONALITY LOGIC:
 *   From the sovereign-states-restrictive seat, the reading permits maximum flexibility and provides a protective floor while respecting state autonomy — d near 0.3 (beneficiary, though not collecting direct rent; benefiting from discretionary power). From the excluded-claimants seats, the same reading operates as a barrier and an extractive mechanism — d near 0.95 (trapped targets). From the asylum-adjudication bodies, the reading is both constraining (legal directives they must apply) and legitimacy-bearing (they conduct individualized review) — d near 0.6 (intermediate: they implement extraction but also carry procedural legitimacy). From the offshore-processing-operators seat, the reading justifies externalization and reduces admissions pressure — d near 0.1 (beneficiary via authorization rather than direct collection). The engine computes these divergent d values per seat from the structural data; the restrictive-reading seat does not experience the constraint as extractive, while the excluded-claimant seats do.
 *
 * MANDATROPHY ANALYSIS:
 *   The restrictive reading avoids classic mandatrophy by referencing its founding problem (totalitarian persecution) as still-live: states claim modern persecution still includes state-directed targeted harm, so the founding mandate persists. However, the reading exhibits zombie characteristics: the founding problem (totalitarian state persecution) is substantially less common than the broader-category displacement (generalized violence, climate-driven, non-state) that modern asylum systems actually face. The restrictive reading has not updated its definition to track the evolved threat environment; instead, it has dug in and expanded justifications (safe third country, non-refoulement carve-outs) to maintain the exclusions. The theater ratio (0.41, rising to 0.41) reflects this: states spend increasing effort defending the exclusions rather than demonstrating that the founding problem persists. The mismatch between founding_problem_status=live and the actual displacement landscape supports a zombie diagnosis: the reading persists not because the founding mandate is live, but because wealthy states benefit from the exclusions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persecution_definition_indeterminacy,
    'What constitutes ''persecution'' — is it limited to targeted, individualized, state-directed harm, or does it include systematic, severe harm regardless of targeting precision or state agency?',
    'Jurisprudential analysis of actual asylum decisions under the restrictive vs. expansive reading; comparison of asylum approval rates for generalized-violence claimants across restrictive and expansive jurisdictions; UNHCR General Comments and treaty interpretation authority.',
    'If the Convention''s drafting history and object-and-purpose analysis support the expansive reading, the restrictive reading is a post-hoc narrowing that violates the Convention''s mandate and reclassifies the constraint as a false mountain or snare masquerading as rope. If the individual-persecution limit is defensible from the text, the restrictive reading''s legitimacy improves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(persecution_definition_indeterminacy, conceptual, 'The boundary between persecution and general hardship remains contested and reading-dependent.').

omega_variable(
    particular_social_group_constitution,
    'Can a ''particular social group'' be constituted by the persecution itself (gender-based violence, LGBTQ+ persecution, family-based violence), or must it pre-exist as an immutable, state-recognized category?',
    'Comparative case law analysis; UNHCR guidance and treaty interpretation; post-hoc observation of claimants excluded by the restrictive definition who would be protected by the expansive definition.',
    'If social groups can be constituted by the persecution claim, millions of gender-based and LGBTQ+ claimants currently excluded by the restrictive reading would gain protection. If immutability-first is required, the restrictive reading''s gate stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(particular_social_group_constitution, conceptual, 'Whether group membership is pre-constituted or instantiated by persecution drives the victim set.').

omega_variable(
    non_state_persecution_scope,
    'Does the Convention require state persecution specifically, or does it cover persecution by non-state actors where the state is unwilling or unable to protect?',
    'Treaty object-and-purpose analysis; UNHCR Handbook (1979, 2019) takes expansive position; comparative state practice; analysis of whether exclusion of non-state persecution contradicts the Convention''s humanitarian rationale.',
    'If non-state persecution is within scope, victims of gang violence, family violence, clan-based violence, and organized crime would qualify across both restrictive and expansive readings. The victim set would expand dramatically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_state_persecution_scope, empirical, 'Whether non-state persecution qualifies remains contested between restrictive and expansive readings.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (active gatekeeping by states, legal doctrine, enforcement machinery) or internalized (self-censorship by claimants who learn the restrictive definitions and abandon claims that fall outside them)?',
    'Post-claim suppression trajectory: if exclusion or denial leads to revised claim-abandonment (claimants reframe their persecution to fit narrow categories), suppression is internalized. If claimants attempt appeals and refuse the narrow framing, suppression is structural.',
    'If internalized, the restrictive reading''s suppression is deeper than the structural measure suggests — claimants carry the internalized exclusion with them even after claim denial. If structural, the barrier is contingent on continued enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the restrictive reading''s exclusion is enforced or internalized affects exit-option credibility and escape-window likelihood.').

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (totalitarian persecution of individuals by states) still live as the primary asylum driver, or has the displacement landscape shifted to generalized violence, environmental collapse, and non-state persecution?',
    'Quantitative analysis of asylum claims by cause (state-targeted persecution vs. generalized violence vs. environmental vs. family-based) across jurisdictions over time; origin-country conflict analysis and displacement causation studies.',
    'If the founding problem is substantially obsolete (modern displacement is mostly non-state, generalized, or environmental), the restrictive reading is a zombie constraint — persisting because states benefit from it, not because the mandate is live. If state persecution remains the largest asylum driver, the founding problem status=live is defensible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the Convention''s founding problem remains the primary asylum driver determines the mandatrophy status of the restrictive reading.').

omega_variable(
    kernel_reading_committer_frame,
    'Is the restrictive reading grounded in a defensible interpretation of the Convention text and intent, or is it a post-hoc reconstruction serving state interests in reducing asylum admissions?',
    'Historical analysis of the 1951 Convention negotiating record; UNHCR interpretation authority and post-1951 jurisprudence; analysis of whether the restrictive reading was the understood meaning in 1951 or emerged later as asylum pressure increased.',
    'If the restrictive reading is a later reconstruction, it is a false-summit reading where the Convention is presented as natural law but is actually serving state interests. If it is a defensible interpretation of the original intent, it remains a legitimate reading even if others disagree.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Whether the restrictive reading is faithful to the Convention''s intent or a post-hoc reconstruction for state benefit determines the false-summit diagnosis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t0, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(refu_tr_t0, observed).
narrative_ontology:measurement(refu_tr_t5, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(refu_tr_t5, observed).
narrative_ontology:measurement(refu_tr_t10, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(refu_tr_t10, observed).
narrative_ontology:measurement(refu_tr_t15, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(refu_tr_t15, observed).
narrative_ontology:measurement(refu_tr_t20, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(refu_tr_t20, observed).
narrative_ontology:measurement(refu_tr_t25, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(refu_tr_t25, observed).
narrative_ontology:measurement(refu_tr_t30, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(refu_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(refu_be_t0, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(refu_be_t0, observed).
narrative_ontology:measurement(refu_be_t5, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(refu_be_t5, observed).
narrative_ontology:measurement(refu_be_t10, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(refu_be_t10, observed).
narrative_ontology:measurement(refu_be_t15, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(refu_be_t15, observed).
narrative_ontology:measurement(refu_be_t20, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(refu_be_t20, observed).
narrative_ontology:measurement(refu_be_t25, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(refu_be_t25, observed).
narrative_ontology:measurement(refu_be_t30, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(refu_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t0, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(refu_su_t0, observed).
narrative_ontology:measurement(refu_su_t5, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(refu_su_t5, observed).
narrative_ontology:measurement(refu_su_t10, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(refu_su_t10, observed).
narrative_ontology:measurement(refu_su_t15, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(refu_su_t15, observed).
narrative_ontology:measurement(refu_su_t20, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(refu_su_t20, observed).
narrative_ontology:measurement(refu_su_t25, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(refu_su_t25, observed).
narrative_ontology:measurement(refu_su_t30, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(refu_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__restrictive_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__procedural_integrity_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, asylum_offshore_processing_externalization).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, migration_burden_sharing_global).

% DUAL FORMULATION NOTE:
% This story (restrictive_sovereignty_reading) is one of three constraint stories decomposing the contested kernel_id=refugee_convention_text. The three readings are: restrictive_sovereignty_reading (this story — Convention as minimum floor, narrow victim set), expansive_humanitarian_reading (Convention as broad protection mandate, wide victim set), and procedural_integrity_reading (Convention as fair-process grounding, outcome-flexible). Each reading instantiates a structurally distinct constraint with different ε values, beneficiary/victim structures, and classifications. They are linked via network.affects_constraints and are not measurable from a single observable — they are different interpretations of the same text held by different parties. Each story carries its own cs_structure.reading_relations and axioms documenting the sibling relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(refugee_convention_text__restrictive_sovereignty_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
