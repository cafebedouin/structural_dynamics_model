% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__immutable_commandment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__immutable_commandment_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: D&C 132 Plural Marriage as Immutable Eternal Commandment Required for Exaltation
 *   domain: religious_law/political_theology/commitment_system
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the eternal_marriage_covenant
 *   kernel: the claim that D&C 132 establishes plural marriage as eternal,
 *   immutable divine law, a strict precondition for the highest degree of
 *   exaltation, admitting no legitimate mechanism of doctrinal revision.
 *   Under this reading alone, federal anti-polygamy pressure after 1862
 *   (Morrill Act) and escalating through the 1880s (Edmunds Act,
 *   Edmunds-Tucker Act) does not create a legitimate occasion for doctrinal
 *   accommodation — it creates a martyrdom constraint, where compliance with
 *   civil law is structurally equivalent to apostasy from binding revelation.
 *   The internal suppression apparatus (church discipline, social ostracism
 *   of dissenters, theological threat of denied exaltation) rises steadily
 *   across the interval as external federal pressure escalates, because this
 *   reading's own logic supplies no off-ramp: the doctrine cannot bend
 *   without abandoning its own immutability premise. This is a distinct
 *   constraint from the sibling readings (prophetic_override_reading,
 *   temporal_accommodation_reading), which this file does not describe or
 *   average against — see kernel_context.
 *
 * KEY AGENTS:
 *   - senior_male_church_hierarchy: agenda_setter/beneficiary (institutional/arbitrage) — administers sealing authority and occupies the top of the resulting hierarchy
 *   - existing_plural_husbands: beneficiary (powerful/constrained) — gains social and eternal standing from the doctrine's persistence
 *   - plural_wives: payer (powerless/trapped) — bears the material and relational cost under compulsion framed as eternal obligation
 *   - daughters_pledged_to_marriage: payer (powerless/trapped) — enters marriage under doctrinal rather than personal authority
 *   - monogamous_members_denied_exaltation: payer (moderate/constrained) — faces a structural exaltation ceiling under this reading
 *   - dissenting_male_members: excluded (moderate/constrained) — has no legitimate internal channel to contest immutability
 *   - federal_government: excluded (institutional/arbitrage) — exerts the primary external pressure but has no standing in the doctrine's own legitimacy criteria
 *   - religious_historians: observer (analytical/analytical) — reconstructs the documentary and social record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.68).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.79).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "D&C 132 Plural Marriage as Immutable Eternal Commandment Required for Exaltation").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law/political_theology/commitment_system").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, 'ac593f51-2464-4ea5-89ba-4d271b981c1e').
narrative_ontology:cs_kernel_codification('ac593f51-2464-4ea5-89ba-4d271b981c1e', fixed_text).
narrative_ontology:cs_authority_grounding('ac593f51-2464-4ea5-89ba-4d271b981c1e', lineage).
narrative_ontology:cs_interpretation_layer_present('ac593f51-2464-4ea5-89ba-4d271b981c1e').
narrative_ontology:cs_reading_relation('ac593f51-2464-4ea5-89ba-4d271b981c1e', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_reading_relation('ac593f51-2464-4ea5-89ba-4d271b981c1e', eternal_marriage_covenant__temporal_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('ac593f51-2464-4ea5-89ba-4d271b981c1e', foundational, revelation_once_given_cannot_be_superseded).
narrative_ontology:cs_axiom_status(revelation_once_given_cannot_be_superseded, overridden).
narrative_ontology:cs_axiom_grounding('ac593f51-2464-4ea5-89ba-4d271b981c1e', revelation_once_given_cannot_be_superseded, theological).
narrative_ontology:cs_axiom('ac593f51-2464-4ea5-89ba-4d271b981c1e', foundational, exaltation_requires_plural_sealing).
narrative_ontology:cs_axiom_status(exaltation_requires_plural_sealing, overridden).
narrative_ontology:cs_axiom_grounding('ac593f51-2464-4ea5-89ba-4d271b981c1e', exaltation_requires_plural_sealing, theological).
narrative_ontology:cs_reference_frame('ac593f51-2464-4ea5-89ba-4d271b981c1e', nauvoo_era_revelatory_settlement).
narrative_ontology:cs_drift_state('ac593f51-2464-4ea5-89ba-4d271b981c1e', post_manifesto_1890, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('ac593f51-2464-4ea5-89ba-4d271b981c1e', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, senior_male_church_hierarchy).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, existing_plural_husbands).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, plural_wives).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, daughters_pledged_to_marriage).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, monogamous_members_denied_exaltation).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, dissenting_male_members).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__immutable_commandment_reading, eternal_progression_doctrine).
narrative_ontology:constraint_vindicates(eternal_marriage_covenant__immutable_commandment_reading, patriarchal_priesthood_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds sealing authority and interprets D&C 132 as binding, unamendable revelation. Administers who may enter plural marriage, adjudicates worthiness, and enforces the doctrine as a precondition of the highest degree of exaltation. Personally occupies the top of the resulting marital hierarchy and can revise interpretation for others without submitting to equivalent scrutiny.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, senior_male_church_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__immutable_commandment_reading, senior_male_church_hierarchy, beneficiary).

% Have already entered plural marriage under the doctrine's authority, gaining social standing, expanded household labor and reproductive capacity, and a theologically guaranteed claim to higher exaltation. Exit would mean renouncing standing marriages, social status, and the eternal reward structure they were promised.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, existing_plural_husbands, beneficiary,
    powerful, generational, constrained, regional).

% Enter marriages under doctrinal compulsion — refusal is framed as risking damnation and eternal separation from family. Bear disproportionate economic precarity, competition for husband's attention and resources among co-wives, and total dependence on a husband's and hierarchy's goodwill. Legal recourse is foreclosed by both civil illegality of their unions and religious sanction against leaving.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, plural_wives, payer,
    powerless, biographical, trapped, local).

% Some are betrothed by family or ecclesiastical arrangement while legally minors or newly of age, with the marriage framed as obedience to eternal law rather than personal choice. Have no independent institutional standing from which to object; refusal risks family and community rupture and threatened loss of salvation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, daughters_pledged_to_marriage, payer,
    powerless, biographical, trapped, local).

% Remain faithful, monogamous members who are told under this reading that the highest degree of glory is structurally unavailable to them without entering plural marriage. Bear a permanent theological second-tier status inside the same community whose leadership benefits from the doctrine's persistence.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, monogamous_members_denied_exaltation, payer,
    moderate, biographical, constrained, regional).

% Object on grounds of conscience, spousal harm, or civil law but have no formal channel within this reading's framework to challenge the doctrine's status as immutable revelation — doing so publicly risks excommunication and community expulsion. Their objections do not appear in official doctrinal deliberation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, dissenting_male_members, excluded,
    moderate, biographical, constrained, regional).

% Criminalizes plural marriage and escalates prosecution, disincorporation threats, and property seizure against the church as an institution. Under this reading, federal law has no legitimate claim to override eternal commandment, so the government's pressure is experienced by the hierarchy as persecution rather than a legitimate input into doctrine, and is excluded from the interpretive process even though it is the primary external force acting on the constraint.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, federal_government, excluded,
    institutional, generational, arbitrage, national).

% Study the documentary record of D&C 132's promulgation, the social structure of plural households, and the later doctrinal transition, without standing to alter the doctrine's status inside the tradition.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__immutable_commandment_reading, senior_male_church_hierarchy).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__immutable_commandment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, textually anchored answer to competing revelatory claims within a young and factionalizing movement, and coordinates a patriarchal kinship and inheritance structure around a single authoritative sealing hierarchy rather than ad hoc arrangements.
% TRANSFER_FUNCTION: Moves reproductive capacity, domestic labor, family loyalty, and theological standing from plural wives and their families to existing husbands and the senior hierarchy that administers sealing authority; moves guaranteed access to the highest eternal reward away from monogamous members and toward those admitted into plural unions.
% ABSENT_VOICES: Plural wives, pledged daughters, and dissenting male members would object to the doctrine's immutable status if given a forum with real institutional standing, but the interpretive authority to declare or revise the doctrine rests entirely with the senior male hierarchy; federal authorities press from outside but are excluded from the doctrine's own legitimacy criteria entirely.
% DISAPPEARANCE_RATIONALE: If the immutable-commandment reading vanished overnight, the entire theological architecture predicated on plural marriage as a precondition for the highest exaltation would collapse: existing plural households would lose their doctrinal warrant, monogamous members would no longer face a structural exaltation ceiling, and the sealing hierarchy's authority to administer this specific practice would dissolve along with it — which is precisely what happened when the prophetic_override_reading and temporal_accommodation_reading displaced this reading's practical force after 1890.
% FOUNDING_PROBLEM: The early movement needed a theological answer to accusations of licentiousness surrounding unofficial plural relationships already occurring among leadership, and needed to establish a durable kinship/priesthood order that could outlast any single leader's authority — D&C 132 supplied a textual, revelatory anchor claimed to resolve both at once.
% FOUNDING_PROBLEM_CORROBORATION: Federal courts, dissenting former members, and independent historians outside the hierarchy attest that the practical problem the doctrine was said to solve (property, community cohesion, succession) was resolved through the 1890 Manifesto's accommodation to civil law rather than through the doctrine's own immutable terms; the hierarchy that promulgated and benefited from the doctrine is the only party still attesting to its live, unrevised status under this specific reading.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__immutable_commandment_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__immutable_commandment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__immutable_commandment_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68 by interval end) reflects the concentration of reproductive, economic, and theological benefit in existing plural husbands and the hierarchy, extracted from plural wives, pledged daughters, and doctrinally second-tier monogamous members. Suppression is authored higher than extraction (0.79) because this specific reading's persistence depends on active, escalating enforcement — internal discipline against dissent and external martyrdom-framing against federal law — not merely on participants' preference. Theater rises modestly (0.10 to 0.28) as the doctrine's practical implementation increasingly required public defense and legal argument rather than routine administration, but remains comparatively low because the underlying coordination and extraction functions were still substantially live, not yet vestigial. Suppression is authored as the raw structural property it is; only extraction is understood as scope/directionality-scaled by the engine.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (hierarchy, existing husbands) receive low derived d — the constraint subsidizes their status and reward structure. Victims (plural wives, pledged daughters, denied-exaltation monogamous members) receive high derived d, amplified by trapped or constrained exit: plural wives in particular have no civil-legal standing for their unions and face religious sanction for leaving, placing them near the full-target end. Dissenting male members sit at moderate power but constrained exit — they can theoretically leave the community but at high relational and social cost, which is why they are marked excluded rather than payer: their objection has no legitimate hearing inside this reading's framework, which is the structurally distinct fact from bearing direct cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem×disappearance_verdict pair is deliberately marked dead/world_rearranges: this reading's own founding problem (anchoring a young movement's authority and answering accusations against existing informal plural unions) was resolved through the temporal_accommodation_reading's 1890 settlement with civil law, not through this reading's own immutable terms holding. That the practice's suspension nonetheless caused the whole structure to rearrange is exactly the signal that the arrangement had become load-bearing for reasons beyond its stated founding purpose — a classic mandatrophy signature, flagged here rather than concealed by an inflated claim that the problem remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_versus_administrative_discretion,
    'Is the ''eternal and immutable'' status of D&C 132 a genuine claim about the text''s ontological status within the tradition, or is it itself an administrative choice by the hierarchy that could, in principle, be revised by the same authority that promulgated it — making ''immutability'' itself contingent on who currently holds sealing authority?',
    'Compare this reading''s textual and institutional claims against the eventual 1890 Manifesto and subsequent doctrinal statements: if the hierarchy that once affirmed immutability later affirms suspension/accommodation without claiming to violate its own prior doctrine, that reveals the immutability claim was administratively revisable all along, which the prophetic_override_reading and temporal_accommodation_reading exist to capture as separate constraints.',
    'If immutability is administratively contingent rather than ontologically fixed, this reading is best understood as one strategically chosen framing among several available to the same authority, rather than a discovered eternal fact — which does not change this story''s own ε or classification, but bears on how much interpretive weight to give the ''no legitimate revision path'' premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_versus_administrative_discretion, conceptual, 'Whether immutability is a structural fact about the doctrine or a strategic framing choice available to the same authority that could revise it.').

omega_variable(
    beneficiary_versus_natural_law_ambiguity,
    'Because this is not authored as a mountain, this omega addresses a related but distinct ambiguity: is the doctrine''s persistence under this reading better explained by sincere theological conviction among the hierarchy (a good-faith reading of revelation) or by the concentrated material and status benefits that persistence confers on exactly the same hierarchy and existing plural husbands?',
    'Examine whether doctrinal defenders who held minority or non-beneficiary positions (e.g., unmarried or monogamous leaders) argued for the doctrine''s immutability with comparable intensity to beneficiaries, and whether internal dissent from beneficiaries themselves occurred at meaningful rates.',
    'If defense of immutability tracks beneficiary status closely, the coordination story (uniting a young movement''s authority structure) functions substantially as cover for concentrated extraction, reinforcing the tangled_rope classification; if defense is evenly distributed across beneficiaries and non-beneficiaries, the coordination function is more substantively independent of the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_versus_natural_law_ambiguity, empirical, 'Whether doctrinal conviction or concentrated material benefit better explains this reading''s persistence among its defenders.').

omega_variable(
    suppression_mechanism_ambiguity,
    'For plural wives and pledged daughters specifically, is the measured suppression primarily structural (civil illegitimacy of their unions, economic dependency, geographic isolation in Utah territory) or partly internalized (belief that resistance risks their own and their family''s eternal damnation, learned from childhood within the tradition)?',
    'Post-1890 trajectory analysis: track whether women who left plural households after the Manifesto''s practical suspension continued to report internalized obligation or guilt absent the structural enforcement, versus reporting relief consistent with purely structural suppression.',
    'If suppression is substantially internalized, the effective suppression these agents experience is higher than the structural measure alone suggests, and persists beyond the interval measured here even after external enforcement eased.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism affecting plural wives and pledged daughters.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 1843, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1843, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1843, 0.1).
narrative_ontology:measurement(eter_tr_t1852, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1852, 0.14).
narrative_ontology:measurement(eter_tr_t1862, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1862, 0.18).
narrative_ontology:measurement(eter_tr_t1874, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1874, 0.22).
narrative_ontology:measurement(eter_tr_t1882, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1882, 0.26).
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1890, 0.28).

% Extraction over time
narrative_ontology:measurement(eter_be_t1843, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1843, 0.42).
narrative_ontology:measurement(eter_be_t1852, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1852, 0.55).
narrative_ontology:measurement(eter_be_t1862, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1862, 0.61).
narrative_ontology:measurement(eter_be_t1874, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1874, 0.66).
narrative_ontology:measurement(eter_be_t1882, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1882, 0.7).
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1890, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1843, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1843, 0.35).
narrative_ontology:measurement(eter_su_t1852, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1852, 0.48).
narrative_ontology:measurement(eter_su_t1862, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1862, 0.6).
narrative_ontology:measurement(eter_su_t1874, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1874, 0.7).
narrative_ontology:measurement(eter_su_t1882, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1882, 0.78).
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1890, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, prophetic_override_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the eternal_marriage_covenant kernel. immutable_commandment_reading (this file) claims D&C 132 as fixed, unrevisable revelation with no legitimate accommodation path. prophetic_override_reading claims continuing revelation permits a living prophet to supersede prior revelation outright. temporal_accommodation_reading claims the eternal principle survives intact while practical obedience yields to civil law. The three readings share the same underlying kernel text but produce structurally different beneficiary/victim configurations, different enforcement postures, and different ε trajectories — they are not the same constraint measured three ways; per the ε-invariance principle they are three constraints linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
