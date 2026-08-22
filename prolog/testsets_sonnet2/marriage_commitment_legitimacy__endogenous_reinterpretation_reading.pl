% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__endogenous_reinterpretation_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
 *   human_readable: The 1890 Manifesto as Genuine Prophetic Revelation Preserving the Church
 *   domain: religious_institutional/political_theology
 *
 * SUMMARY:
 *   This story instantiates the endogenous_reinterpretation_reading of the
 *   marriage_commitment_legitimacy kernel: the 1890 Manifesto ending the
 *   sanctioning of new plural marriages is read, from within the tradition's
 *   own commitment structure, as genuine prophetic revelation — God directing
 *   the church president to reverse an earlier revealed practice in order to
 *   preserve the institution for its larger purposes. Under this reading the
 *   federal government's Edmunds-Tucker prosecution is a catalyst and
 *   occasion, not the operative cause; the theological throughline
 *   (continuous revelation, unbroken prophetic succession) is preserved by
 *   treating the reversal as a new stage of covenant rather than a break or a
 *   capitulation. Extraction on this reading is low: the arrangement mainly
 *   functions as legitimacy-preserving coordination for the institution and
 *   the community that remained within it. Costs are borne narrowly, by the
 *   plural families whose domestic arrangements were dissolved and by
 *   fundamentalist dissenters who held the prior revelation as permanent and
 *   were marginalized for continuing to act on it. Sibling readings
 *   (exogenous_override_reading, hybrid_pragmatic_reading) author the same
 *   underlying event with substantially higher extraction and a different
 *   account of what did the causal work; see cs_structure.reading_relations
 *   for how this reading relates to each.
 *
 * KEY AGENTS:
 *   - church_hierarchy: agenda_setter/beneficiary (institutional/arbitrage) — issues and administers the reversal as revelation
 *   - current_apostolic_leadership: beneficiary (institutional/arbitrage) — inherits legitimacy from the unbroken-succession reading
 *   - plural_families_dissolved_by_the_reversal: payer (powerless/trapped) — bears the domestic and social cost
 *   - fundamentalist_dissenters: payer (powerless/trapped) — marginalized for holding the prior revelation as permanent
 *   - federal_government: excluded (institutional/analytical) — the coercive pressure this reading treats as mere occasion
 *   - church_historians: observer (moderate/constrained) — assesses the archival record with only partial independent access
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.18).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.32).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "The 1890 Manifesto as Genuine Prophetic Revelation Preserving the Church").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "religious_institutional/political_theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'a9cea659-6f95-4dff-9fc2-94326c0a9d95').
narrative_ontology:cs_kernel_codification('a9cea659-6f95-4dff-9fc2-94326c0a9d95', formalized).
narrative_ontology:cs_authority_grounding('a9cea659-6f95-4dff-9fc2-94326c0a9d95', lineage).
narrative_ontology:cs_interpretation_layer_present('a9cea659-6f95-4dff-9fc2-94326c0a9d95').
narrative_ontology:cs_reading_relation('a9cea659-6f95-4dff-9fc2-94326c0a9d95', marriage_commitment_legitimacy__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('a9cea659-6f95-4dff-9fc2-94326c0a9d95', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('a9cea659-6f95-4dff-9fc2-94326c0a9d95', foundational, revelation_can_supersede_prior_revelation).
narrative_ontology:cs_axiom_status(revelation_can_supersede_prior_revelation, holdable).
narrative_ontology:cs_axiom_grounding('a9cea659-6f95-4dff-9fc2-94326c0a9d95', revelation_can_supersede_prior_revelation, theological).
narrative_ontology:cs_axiom('a9cea659-6f95-4dff-9fc2-94326c0a9d95', secondary, external_pressure_is_occasion_not_cause).
narrative_ontology:cs_axiom_status(external_pressure_is_occasion_not_cause, holdable).
narrative_ontology:cs_axiom_grounding('a9cea659-6f95-4dff-9fc2-94326c0a9d95', external_pressure_is_occasion_not_cause, conventional).
narrative_ontology:cs_reference_frame('a9cea659-6f95-4dff-9fc2-94326c0a9d95', continuous_revelation_unbroken_succession).
narrative_ontology:cs_drift_state('a9cea659-6f95-4dff-9fc2-94326c0a9d95', post_manifesto_canonization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a9cea659-6f95-4dff-9fc2-94326c0a9d95', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_succession_doctrine).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, mainstream_membership).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, current_apostolic_leadership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, plural_families_dissolved_by_the_reversal).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, fundamentalist_dissenters).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, continuous_revelation_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_authority_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and administers the Manifesto as a revealed instruction, incorporating it into canon and using it to negotiate statehood and end federal prosecution. Retains full authority to interpret its scope and later extend the reasoning (further restrictions in 1904) without conceding the original revelation was anything but genuine.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_hierarchy, beneficiary).

% Inherits an unbroken line of prophetic succession whose legitimacy depends on the 1890 reversal being read as genuine revelation rather than capitulation. A capitulation reading would destabilize the same authority structure current leaders now exercise.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, current_apostolic_leadership, beneficiary,
    institutional, civilizational, arbitrage, global).

% Receives a coherent faith narrative in which the church's history is one of continuous, trustworthy revelation rather than expedient reversal. Benefits from social respectability, statehood, and cessation of federal persecution that followed the Manifesto.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, mainstream_membership, beneficiary,
    organized, generational, constrained, national).

% Existing plural wives and children faced sudden loss of legal, social, and religious legitimacy for their family structure. Many families were quietly broken apart, some wives and children left unprovided for, as the institution reoriented around monogamy as doctrine going forward.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, plural_families_dissolved_by_the_reversal, payer,
    powerless, biographical, trapped, regional).

% Members who held that the original revelation on plural marriage was eternal and unconditional were excommunicated or marginalized when they refused to accept the reversal, losing standing within the very institution whose continuity the Manifesto was said to preserve.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, fundamentalist_dissenters, payer,
    powerless, generational, trapped, regional).

% The doctrine that the church president speaks for God and can receive binding revelation is itself vindicated and strengthened by a reading in which the Manifesto is genuine prophecy rather than political retreat. Listed as a non-agent entity that benefits structurally without being a person who collects anything.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_succession_doctrine, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_succession_doctrine).

% Applied the Edmunds-Tucker Act and threatened disincorporation and property seizure, creating the external pressure the endogenous reading treats as mere occasion rather than cause. Under this reading, the federal government's coercive role is minimized to a backdrop God used, not a determining force; the government itself has no voice in how the church narrates its own revelation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, federal_government, excluded,
    institutional, biographical, analytical, national).

% Study the archival record of the Manifesto's drafting, timing, and internal deliberation. Their access to church archives is itself partially controlled by the institution whose founding narrative they are evaluating, constraining independent corroboration.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_historians, observer,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, current_apostolic_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a theological mechanism for the institution to change core practice (ending plural marriage) while preserving the doctrine of continuous revelation and unbroken prophetic authority that binds the whole religious community together.
% TRANSFER_FUNCTION: Moves legitimacy and institutional continuity toward current and future church leadership and toward the mainstream membership's respectable public identity, while moving social, familial, and religious standing away from plural families and fundamentalist believers who trusted the prior revelation as permanent.
% ABSENT_VOICES: Plural wives and their children, whose domestic and legal security depended on the prior doctrine, are not consulted as a body in the Manifesto's issuance; fundamentalist members who dispute the reversal's genuineness are excluded from the institution once they act on their dissent, and federal officials are excluded from any voice in how the church characterizes the revelation's authenticity.
% DISAPPEARANCE_RATIONALE: If the endogenous-revelation reading collapsed as a live commitment inside the tradition, the entire chain of prophetic succession legitimacy resting on it would be exposed to reinterpretation; current leadership's authority, temple ordinances tied to unbroken revelation, and the institution's self-narrative of continuous divine guidance would all require renegotiation.
% FOUNDING_PROBLEM: The church faced federal prosecution, disincorporation, and property confiscation over plural marriage, and needed a way to end the practice without conceding that its founding prophet's revelation on the subject had been wrong or that prophetic authority could be overridden by secular force.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians outside the church's benefiting leadership structure, examining the Edmunds-Tucker Act's timing and the Manifesto's drafting correspondence, corroborate that the institutional survival problem was real and severe; the church's own continued citation of prophetic succession as the reason for and validation of the reversal is self-asserted from within the benefiting hierarchy and is not independently corroborated as the operative cause.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).
:- end_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18-0.28) because, taken on its own terms, this reading describes a coordination structure that preserves the community's continuity and shields the great majority of members from further federal persecution — the primary transfer is legitimacy and institutional survival, not material extraction from a broad victim class. Suppression is moderate (0.32) reflecting the real marginalization of dissenters who rejected the reversal, but this is authored as a bounded, targeted cost rather than a broad extractive mechanism. Theater ratio starts moderate (0.35) reflecting genuine performative elements in how the revelation was publicly framed and declines over the interval (to 0.22) as the doctrine settled into unquestioned institutional memory and required less active narrative maintenance. Accessibility collapse (0.45) and resistance (0.4) are mid-range, appropriate to a rope rather than a mountain: the endogenous reading is a contestable interpretive commitment, not a natural law, and alternative readings (the sibling constraints) remain live in scholarly and dissenting discourse even though they have been substantially foreclosed within the mainstream institution's own self-understanding.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter and beneficiary seats (church_hierarchy, current_apostolic_leadership, mainstream_membership), the arrangement computes as continuity-preserving coordination: a genuine revelation solving a genuine institutional crisis. From the payer seats (plural_families_dissolved_by_the_reversal, fundamentalist_dissenters), the same structural event computes very differently — a doctrine reversal that could not be questioned without loss of standing, imposed on people who had organized their lives around the prior revelation's claimed permanence. The engine should register this divergence as the structural signature of a contested kernel reading rather than as an error in either seat's computation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (church_hierarchy, current_apostolic_leadership, mainstream_membership, prophetic_succession_doctrine) sit near the low-d end: the reading directly subsidizes their legitimacy and continuity. Victims (plural_families_dissolved_by_the_reversal, fundamentalist_dissenters) sit near the high-d end: they bear concentrated, non-consensual costs from a doctrinal change they had no power to contest from within the institution's own epistemic frame. Federal_government is excluded rather than positioned on the beneficiary/victim axis at all — under this reading its coercive role is deliberately minimized to catalyst status, which is itself the interpretive move this reading makes and the reason it is excluded from voice in six_questions.absent_voices.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (federal prosecution threatening institutional survival) is authored as founding_problem_status: live at the time of issuance, but the reading's own claim is that the deeper problem the revelation solved — how to preserve prophetic authority's inviolability through a doctrinal change — is answered permanently by treating the change itself as revealed. This is precisely the kind of self-sealing genealogy the R5 corroboration rule is built to test: the church's own account is not independently corroborated as the operative cause, only the external survival pressure is. The mismatch between a self-asserted 'genuine revelation' narrative and independently corroborated 'severe external pressure' evidence is the signal this story is authored to expose, without pre-resolving it in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_reading_committer_structure,
    'Is the Manifesto''s genuineness-as-revelation claim a live, independently defensible theological position, or is it structurally indistinguishable from a legitimating cover story generated after the fact by the very authority it vindicates?',
    'Compare pre-1890 internal church correspondence and sermons asserting plural marriage''s permanence against post-1890 statements; a sharp discontinuity with no anticipatory theological groundwork prior to the Edmunds-Tucker crisis would weaken the endogenous reading''s claim to genuineness independent of the pressure.',
    'If the pre-crisis record shows no theological groundwork anticipating a reversal, this reading''s low-extraction classification becomes harder to sustain independent of the exogenous_override_reading''s account, and the two readings'' ε values would need re-examination even though they remain separate constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_reading_committer_structure, conceptual, 'Whether the endogenous revelation claim is independently defensible or a retrospective legitimation generated by the same authority it vindicates.').

omega_variable(
    beneficiary_versus_natural_continuity,
    'Does prophetic_succession_doctrine benefit from this reading as a genuine natural continuation of established practice, or is the doctrine itself substantially reconstituted by the Manifesto in a way that makes calling it a ''beneficiary'' already assume the reading''s conclusion?',
    'Trace whether the specific claim that revelation can reverse prior revelation was an established pre-1890 doctrinal feature or was itself first articulated to justify the Manifesto.',
    'If the reversal-capacity doctrine was newly articulated in 1890, prophetic_succession_doctrine''s beneficiary status is partly circular — the doctrine benefits from a reading that also, in part, invented the doctrine''s current shape.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_versus_natural_continuity, conceptual, 'Circularity risk in treating the succession doctrine as a pre-existing beneficiary rather than a co-constructed element of this specific reading.').

omega_variable(
    sibling_reading_causal_weighting,
    'What relative causal weight should be assigned to federal coercion versus internal theological development in producing the 1890 reversal, and does that weighting differ depending on which reading''s own lights are used to assess it?',
    'This is inherently a question of interpretive framework rather than a single empirical fact — historians across all three readings agree on the documentary timeline but weight causation differently. Resolution would require either a change in the church''s own doctrinal self-description or new archival material showing internal deliberation processes.',
    'Determines which of the three sibling readings (this one, exogenous_override_reading, or hybrid_pragmatic_reading) best fits the documentary record, but does not change that each remains a separately authored constraint with its own ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_causal_weighting, conceptual, 'The causal-weighting question that separates the three kernel readings from one another.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t22, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 22, 0.3).
narrative_ontology:measurement_basis(marr_tr_t22, observed).
narrative_ontology:measurement(marr_tr_t44, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 44, 0.27).
narrative_ontology:measurement_basis(marr_tr_t44, observed).
narrative_ontology:measurement(marr_tr_t66, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 66, 0.25).
narrative_ontology:measurement_basis(marr_tr_t66, observed).
narrative_ontology:measurement(marr_tr_t88, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 88, 0.23).
narrative_ontology:measurement_basis(marr_tr_t88, observed).
narrative_ontology:measurement(marr_tr_t110, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 110, 0.22).
narrative_ontology:measurement_basis(marr_tr_t110, observed).
narrative_ontology:measurement(marr_tr_t130, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 130, 0.22).
narrative_ontology:measurement_basis(marr_tr_t130, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t22, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 22, 0.24).
narrative_ontology:measurement_basis(marr_be_t22, observed).
narrative_ontology:measurement(marr_be_t44, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 44, 0.2).
narrative_ontology:measurement_basis(marr_be_t44, observed).
narrative_ontology:measurement(marr_be_t66, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 66, 0.19).
narrative_ontology:measurement_basis(marr_be_t66, observed).
narrative_ontology:measurement(marr_be_t88, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 88, 0.18).
narrative_ontology:measurement_basis(marr_be_t88, observed).
narrative_ontology:measurement(marr_be_t110, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 110, 0.18).
narrative_ontology:measurement_basis(marr_be_t110, observed).
narrative_ontology:measurement(marr_be_t130, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 130, 0.18).
narrative_ontology:measurement_basis(marr_be_t130, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% Part of the marriage_commitment_legitimacy kernel family (3 readings). This story (endogenous_reinterpretation_reading) authors low ε (~0.18-0.28) reflecting genuine-revelation framing with narrow, bounded victim classes. exogenous_override_reading authors substantially higher ε reflecting coercion-under-duress framing with a broader victim class including the institution itself as coerced party. hybrid_pragmatic_reading authors intermediate ε reflecting strategic-adaptation framing. All three describe the same 1890 Manifesto event; they are linked here rather than merged because each instantiates a structurally distinct claim about what happened and who bears the costs of that claim being institutionally accepted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
