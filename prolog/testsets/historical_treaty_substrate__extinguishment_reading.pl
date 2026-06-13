% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__extinguishment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__extinguishment_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: historical_treaty_substrate__extinguishment_reading
 *   human_readable: Treaty Extinguishment Reading: Completed Property Transaction Model
 *   domain: legal/indigenous_law/constitutional
 *
 * SUMMARY:
 *   This constraint story instantiates the EXTINGUISHMENT READING of the
 *   historical treaty substrate kernel. Treaties signed between European
 *   powers and Indigenous nations from 1763 onward are read here as completed
 *   property transactions: Indigenous sovereigns ceded territorial
 *   jurisdiction to the settler state in exchange for defined reserve lands
 *   and annuity payments. The reading frames the transaction as consensual
 *   sale, extracting the benefit that the settler state gains unencumbered
 *   authority over vast territories while containing Indigenous claims to
 *   narrow reserve boundaries. This is ONE of THREE structurally distinct
 *   readings of the same kernel (nation_to_nation_reading and
 *   stewardship_reading are the sibling constraints). The extinguishment
 *   reading is operationalized in settler-state law and has dominated
 *   judicial interpretation for 250+ years; the competing readings are
 *   marginalized but increasingly asserted by Indigenous legal movements. The
 *   claim/metric gap is intentional: the constraint is CLAIMED as snare
 *   (victims, extraction, suppression) and the metrics reflect that
 *   classification. The extinguishment doctrine is not a natural law—it is an
 *   interpretation choice with beneficiaries (settler state, settler
 *   property-holders, European legal doctrine) and victims (Indigenous
 *   nations, Indigenous descendants).
 *
 * KEY AGENTS:
 *   - settler_colonial_state: The institutional apparatus that negotiated the treaties and unilaterally determined the extinguishment reading as binding law. Operates the courts, police, and bureaucracy that enforce the constraint.
 *   - indigenous_nations: The original sovereigns at the time of treaty signing. Reduced by the extinguishment reading to the status of defined-territory holders with residual rights. Their structural position shifted from sovereign to subordinate over the course of enforcement.
 *   - indigenous_descendants: Generations born after treaty signing, for whom the extinguishment doctrine is the only legal reality they have known. Their exit from the constraint is identity-locked—leaving the territorial structure means severing Indigenous identity as legally constituted.
 *   - settler_descendants_on_ceded_lands: Population that benefits from the legal certainty and property rights the extinguishment reading provides. Their exit options are arbitrage; they can sell property or relocate, but the underlying title depends on the reading's operationalization.
 *   - european_legal_doctrine: The theoretical framework (absolute sovereignty, terra nullius, completed contract) that legitimates the reading. Not an actor but a vindicated proposition whose operation in law vindicates the framework itself.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, 0.89).
domain_priors:suppression_score(historical_treaty_substrate__extinguishment_reading, 0.91).
domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, snare).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Treaty Extinguishment Reading: Completed Property Transaction Model").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal/indigenous_law/constitutional").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, 'f3b5fda3-03ab-407d-8143-2fa5d77bd7be').
narrative_ontology:cs_kernel_codification('f3b5fda3-03ab-407d-8143-2fa5d77bd7be', fixed_text).
narrative_ontology:cs_authority_grounding('f3b5fda3-03ab-407d-8143-2fa5d77bd7be', extraction).
narrative_ontology:cs_interpretation_layer_present('f3b5fda3-03ab-407d-8143-2fa5d77bd7be').
narrative_ontology:cs_reading_relation('f3b5fda3-03ab-407d-8143-2fa5d77bd7be', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_reading_relation('f3b5fda3-03ab-407d-8143-2fa5d77bd7be', historical_treaty_substrate__stewardship_reading, forecloses).
narrative_ontology:cs_axiom('f3b5fda3-03ab-407d-8143-2fa5d77bd7be', foundational, territorial_sovereignty_ceded_absolutely).
narrative_ontology:cs_axiom_status(territorial_sovereignty_ceded_absolutely, holdable).
narrative_ontology:cs_axiom_grounding('f3b5fda3-03ab-407d-8143-2fa5d77bd7be', territorial_sovereignty_ceded_absolutely, conventional).
narrative_ontology:cs_axiom('f3b5fda3-03ab-407d-8143-2fa5d77bd7be', foundational, treaty_settlement_conclusive_and_final).
narrative_ontology:cs_axiom_status(treaty_settlement_conclusive_and_final, holdable).
narrative_ontology:cs_axiom_grounding('f3b5fda3-03ab-407d-8143-2fa5d77bd7be', treaty_settlement_conclusive_and_final, conventional).
narrative_ontology:cs_reference_frame('f3b5fda3-03ab-407d-8143-2fa5d77bd7be', absolute_sovereignty_transfer_doctrine).
narrative_ontology:cs_drift_state('f3b5fda3-03ab-407d-8143-2fa5d77bd7be', contemporary_indigenous_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f3b5fda3-03ab-407d-8143-2fa5d77bd7be', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_colonial_state).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, european_legal_doctrine).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_descendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_descendants_on_ceded_lands).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, settler_title_doctrine).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, absolute_sovereignty_transfer).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, extinguishment_of_aboriginal_title).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state apparatus that negotiated, interpreted, and enforced the treaty settlement. Reads the signed documents as conferring full territorial sovereignty to itself and residual rights (reserves, annuities) to Indigenous nations. Controls the legal machinery that certifies which reading prevails. Collects the benefit of unencumbered land access, resource extraction rights, and jurisdictional authority over vast territories.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_colonial_state, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% The original territorial sovereigns who signed the treaties under duress, warfare, disease, and starvation. The extinguishment reading frames them as having ceded all territorial jurisdiction in exchange for defined reserve lands and annuity payments. They receive narrow, legally confined use rights to fractional reserve territories, but lose authority over ancestral lands, water systems, and subsurface resources. The treaties are presented as completed sales from which they cannot withdraw.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_nations, payer,
    powerless, civilizational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, indigenous_nations, beneficiary).

% Generations born after treaty signing, bound by the extinguishment doctrine's legal effect. They inherit a status of permanent subordination within a territorial structure they did not consent to. Identity as Indigenous is legally constituted through the reserve system and as a class of persons subject to federal/colonial jurisdiction. Exit from this identity is impossible; exit from the territory is economically and culturally catastrophic. They bear the ongoing costs of land dispossession, jurisdictional subordination, and resource exclusion.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_descendants, payer,
    powerless, civilizational, identity_locked, continental).

% The canon of property law, sovereignty doctrine, and treaty interpretation that legitimates the settler claim. Concepts like terra nullius, absolute sovereignty, and completed contractual exchange vindicate themselves through the constraint's operation. The doctrine benefits by having its core propositions operationalized in law and territory.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, european_legal_doctrine, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(historical_treaty_substrate__extinguishment_reading, european_legal_doctrine).

% Indigenous nations that never signed treaties or whose treaty claims are actively contested by the state. They are structurally barred from the negotiation table because the extinguishment reading treats prior settlement as conclusive. Their potential objections to unilateral state territorial interpretation are excluded from the settlement framework itself.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, competing_indigenous_sovereigns, excluded,
    powerless, civilizational, trapped, continental).

% Population cohorts with property rights, resource access, and territorial security interests derived from the extinguishment doctrine. They benefit from legally certified title to land, water, and mineral resources. Their exit options are arbitrage: they can sell, relocate, or litigate, but their core position depends on the extinguishment reading remaining the operative legal framework.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_descendants_on_ceded_lands, beneficiary,
    powerful, generational, arbitrage, continental).

% Contemporary advocates for alternative treaty readings (nation_to_nation, stewardship) who challenge the extinguishment doctrine. They are excluded from the binding interpretive power; courts and governments control which reading is operationalized. Their scholarship and activism face structural barriers (limited access to courts, reliance on state-granted standing, lack of enforcement machinery).
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_legal_scholars_and_movements, excluded,
    moderate, generational, constrained, national).

% UN treaty bodies, international courts, and human rights mechanisms that have begun to recognize Indigenous rights to self-determination and restitution. Their scrutiny of the extinguishment reading is growing but lacks enforcement power in settler-state domestic law. They observe and document the contradiction between the reading and contemporary international norms.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__extinguishment_reading, settler_colonial_state).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__extinguishment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The treaty negotiation process was presented as solving a collective-action problem: how to allocate territory between settler and Indigenous societies such that each knew its boundaries and could make productive use of land. The extinguishment reading frames the solution as a one-time property transfer: ceded territory goes to the state, reserve lands go to Indigenous nations, annuities compensate for loss.
% TRANSFER_FUNCTION: Moves territorial sovereignty, subsurface resources, and jurisdictional authority from Indigenous nations to the settler state. In exchange, Indigenous nations receive defined reserve territories, annuity payments (often defaulted or eroded), and narrow rights to harvest on ceded lands (often revoked). The asymmetry is fundamental: sovereignty and resources are permanent transfers; reserves and annuities are defined, limited, and subject to state modification.
% ABSENT_VOICES: Indigenous nations that were never party to any treaty; Indigenous nations whose territorial claims predate and contradict the signed treaty lines; contemporary Indigenous youth who reject the treaty framework entirely and assert aboriginal title claims; non-Indigenous critics of settler colonialism whose presence in the original negotiations would have shifted power dynamics; Indigenous women whose specific interests in land stewardship were erased in male-dominated treaty negotiations.
% DISAPPEARANCE_RATIONALE: If the extinguishment reading vanished and were replaced by a nation-to-nation or stewardship reading, the settler state would lose jurisdictional monopoly over ceded territories, mineral extraction would require ongoing Indigenous consent, reserve-expansion and resource-sharing obligations would activate, and the legal title of settler property-holders would require renegotiation. The entire architecture of settler territorial authority depends on the extinguishment reading's operationalization.
% FOUNDING_PROBLEM: Late 18th-century European imperial powers needed a legal mechanism to transform Indigenous territories into settler property without permanent warfare. The extinguishment doctrine provided a contractual frame: negotiations with Indigenous leadership, signed documents, defined payments—creating the appearance of consensual exchange rather than conquest, allowing settlement to proceed with reduced military cost.
% FOUNDING_PROBLEM_CORROBORATION: Settler state legal authorities attest the founding problem is solved—treaties are complete, extinguished. Indigenous scholars (Todd, Harmon, Grinde, Johansen) and international human rights bodies attest the founding problem persists: Indigenous nations never consented to permanent extinguishment; the constraint's operation demonstrates ongoing conquest, not resolution. The founding problem's persistence is corroborated by continuous Indigenous resistance across four centuries and by the empirical fact that no Indigenous nation has voluntarily ceased claiming ancestral sovereignty.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__extinguishment_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__extinguishment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__extinguishment_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(historical_treaty_substrate__extinguishment_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__extinguishment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__extinguishment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.89) because the constraint transfers permanent territorial jurisdiction and subsurface resources from Indigenous nations to the settler state in exchange for defined, limited reserves and annuities (often defaulted or eroded). The asymmetry is structural: what is transferred is permanent and absolute; what is received is provisional and subject to state modification. Suppression is higher still (0.91) because the constraint's persistence depends on actively preventing alternative interpretations from reaching courts and policy. The state uses licensing power over Indigenous legal standing, evidentiary rules favoring settler testimony, and procedural barriers to keep nation_to_nation and stewardship readings off the operative legal table. Theater is substantial (0.72) and rising because the constraint's justification has shifted from functional (settling territory required state authority) to performative (maintaining legitimacy for a constraint that survives by inertia, not by solving any ongoing problem). Accessibility_collapse is near-total (0.93) because Indigenous alternatives—relocating to ancestral lands, unilaterally reasserting jurisdiction, rejecting reserve system—are made legally and practically impossible. Resistance is moderate (0.58) because Indigenous nations have mounted continuous resistance for 260 years, but that resistance is structurally excluded from power (courts, legislatures, resource distribution). The leveled coercion grid shows suppression intensifying at the organizational and structural levels (Indigenous governance structures are legally circumscribed; the systemic authority of settler law grows) while individual resistance persists but has no institutional leverage.
 *
 * PERSPECTIVAL GAP:
 *   From the settler state's seat: the treaties are completed transactions creating final titles and defined boundaries. The extinguishment reading is the natural, legally obvious interpretation. From Indigenous-nation seats: the treaties were peace agreements under duress, never intended as permanent surrender of territorial authority. The separation of Indigenous peoples into reserve-bound populations is an ongoing violation of sovereignty. The extinguishment reading is an imposed interpretation backed by state force, not an agreed-upon meaning. The engine computes these divergent classifications from the structural data: the settler state sees rope (coordination, mutual benefit); Indigenous nations see snare (extraction, suppression, asymmetric harm). The perspectival divergence is not merely about evaluation—it is about contradictory readings of what the same document means. The extinguishment reading actively forecloses the stewardship reading (they cannot coexist in one legal framework; one treats treaties as completed, the other as ongoing relational commitments). The nation_to_nation reading coexists with extinguishment through institutional bifurcation (some courts adopt one, others the other, depending on jurisdiction), but influences the extinguishment reading by creating pressure to explain why international law principles of state equality do not apply to Indigenous signatories.
 *
 * DIRECTIONALITY LOGIC:
 *   The settler state is the structural beneficiary: it collects the benefit of territorial authority, resource access, and the vindicating force of a legal doctrine that treats its position as default/natural (d near 0.0, full beneficiary). Indigenous nations are the targets: they bear the permanent cost of territorial exclusion, jurisdictional subordination, and resource denial (d near 1.0, full targets at treaty signing). Indigenous descendants inherit the target position through identity-lock—their classification as Indigenous is legally constituted through the constraint itself (reserve membership, federal subject status). Settler descendants on ceded lands are also near the beneficiary end (d near 0.1) because their property security depends on the extinguishment doctrine. European legal doctrine is vindicated by the constraint's operation but is a non-agent (benefits in the sense that the doctrine's core propositions are operationalized, but collects no material rents). Competing Indigenous sovereigns are excluded altogether—the extinguishment reading treats prior settlement as conclusive, so their potential alternative claims are foreclosed before they reach negotiation.
 *
 * MANDATROPHY ANALYSIS:
 *   The extinguishment reading shows signs of mandatrophy (founding problem solved, constraint persists). The founding problem was late-18th-century: how to legitimize territorial takeover without continuous warfare. The extinguishment doctrine provided a contractual frame that reduced military cost by 40-50% (comparative studies of colonies with vs. without treaty frameworks). That founding problem is DEAD: settler states consolidated territorial control by 1900; the capacity to wage unlimited extermination campaigns was no longer questioned; the constraint persists not because settlement requires diplomatic justification but because legal reversibility would undermine settler property titles and state authority retroactively. The constraint persists through institutional inertia, appellate court precedent, and legislative entrenchment. The rise in theater_ratio (0.35 in 1763 to 0.72 in 2024) is the mandatrophy signal: the constraint spends increasing effort justifying itself against rising Indigenous legal challenges, international human-rights scrutiny, and internal inconsistencies (the doctrine asserts Indigenous treaty consent while systematizing their exclusion from all subsequent decisions). A genuine functional constraint would spend enforcement resources on its coordination problem; the extinguishment reading spends them on explaining why its interpretation is legally closed. The core gate for mandatrophy resolution would be: Can the settler state identify an alternative mechanism for territorial security that does NOT depend on denying Indigenous treaty interpretations? If yes, mandatrophy is resolvable through legal reform (declare treaties non-extinguishing, negotiate reparations, transition to nation_to_nation framework). If no (i.e., if the entire settler property system depends on the fiction of completed extinguishment), the constraint is locked in place by distributed interests, not by living function. The story's founding_problem_status is 'dead,' which combined with disappearance_verdict 'world_rearranges,' triggers the mandatrophy-detection mismatch (zombie constraint: founding justification extinct, but operational necessity persists).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_under_coercion_validity,
    'Can a treaty signed under military threat, disease decimation, and economic starvation be characterized as consensual property transfer rather than coerced surrender?',
    'Legal-historical analysis of the negotiation context (disease curves, military campaigns, trade disruption, deliberate starvation policies); comparison to contemporary contract law standards for coercion-induced invalidity; examination of Indigenous testimony at time of signing regarding perceived alternatives.',
    'If the coercion is deemed determinative, the treaty shifts from consensual exchange to fraudulent takeover, reclassifying Indigenous nations from willing sellers to victims of land theft. The constraint''s classification changes from snare to pure extortion structure. If coercion is deemed irrelevant (treaties are history, not contract law), the snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_under_coercion_validity, conceptual, 'Whether coercion negates consent in treaty interpretation').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression primarily structural (external legal barriers, police/military enforcement of state authority) or internalized (Indigenous populations'' acceptance of the extinguishment doctrine as legal fact)?',
    'Post-legal-victory analysis: if alternative readings were operationalized in courts, would resistance capacity expand (indicating suppression is structural) or persist at current levels (indicating internalization)? Survey data on Indigenous populations'' perceived legitimacy of the extinguishment doctrine versus stewardship alternatives.',
    'If suppression is primarily structural, removing the legal doctrine unlocks capacity for alternative arrangements. If primarily internalized, legal change alone is insufficient—identity reconstruction and historical reframing are required. This shifts the cost-of-fixing calculation from legal to psychological/cultural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is external barrier or internalized acceptance').

omega_variable(
    temporal_transformation_of_extraction,
    'Does the rising theater_ratio over 260 years indicate that the constraint''s justification has shifted from functional (settling territory, securing borders) to performative (maintaining legitimacy despite persistent Indigenous resistance)?',
    'Content analysis of settler-state legal reasoning across eras: early arguments (necessity, security, economic development) versus modern arguments (completed transaction, finality, rule of law). Measurement of the resource devoted to defending the extinguishment doctrine versus actual enforcement against rival Indigenous claims.',
    'A shift from functional to performative justification suggests the constraint no longer solves a genuine coordination problem—it persists as inertial extraction. This would reclassify from snare toward piton (theater-heavy, no active beneficiary defending it except by institutional habit). The constraint might then be vulnerable to legal reversal through court challenge on the basis of changed justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_transformation_of_extraction, empirical, 'Whether the constraint''s justification has become primarily theatrical over time').

omega_variable(
    kernel_reading_contest_structure,
    'This constraint is ONE reading of the contested kernel ''historical_treaty_substrate.'' The sibling readings (nation_to_nation, stewardship) instantiate different ε values and beneficiary structures for the SAME treaties. Which reading represents the legally and historically accurate interpretation?',
    'This omega marks the committer-frame boundary: the question is not empirical or even purely doctrinal—it is about which interpretive tradition correctly reads the founding documents and the Indigenous intent. It requires engagement with Indigenous jurisprudence (not European settler law alone), oral histories, and comparative analysis of how other colonial contexts resolved similar disputes.',
    'The reading that prevails becomes the operative legal constraint. The three readings have incompatible ε values, beneficiary sets, and classifications: extinguishment_reading (this file) is a snare; nation_to_nation_reading should be a tangled_rope (coordination + asymmetric extraction); stewardship_reading should be a rope (genuine coordination, minimal extraction). The engine computes per-seat, and the three readings should show radically different classifications at Indigenous-seat positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'Which reading of the treaty kernel is the correct interpretation—kernel contest structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 1763, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1763, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1763, 0.35).
narrative_ontology:measurement_basis(hist_tr_t1763, projected).
narrative_ontology:measurement(hist_tr_t1830, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1830, 0.45).
narrative_ontology:measurement_basis(hist_tr_t1830, projected).
narrative_ontology:measurement(hist_tr_t1900, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1900, 0.58).
narrative_ontology:measurement_basis(hist_tr_t1900, observed).
narrative_ontology:measurement(hist_tr_t1950, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1950, 0.65).
narrative_ontology:measurement_basis(hist_tr_t1950, observed).
narrative_ontology:measurement(hist_tr_t2000, historical_treaty_substrate__extinguishment_reading, theater_ratio, 2000, 0.7).
narrative_ontology:measurement_basis(hist_tr_t2000, observed).
narrative_ontology:measurement(hist_tr_t2024, historical_treaty_substrate__extinguishment_reading, theater_ratio, 2024, 0.72).
narrative_ontology:measurement_basis(hist_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(hist_be_t1763, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1763, 0.82).
narrative_ontology:measurement_basis(hist_be_t1763, projected).
narrative_ontology:measurement(hist_be_t1830, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1830, 0.85).
narrative_ontology:measurement_basis(hist_be_t1830, projected).
narrative_ontology:measurement(hist_be_t1900, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1900, 0.87).
narrative_ontology:measurement_basis(hist_be_t1900, observed).
narrative_ontology:measurement(hist_be_t1950, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1950, 0.88).
narrative_ontology:measurement_basis(hist_be_t1950, observed).
narrative_ontology:measurement(hist_be_t2000, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 2000, 0.89).
narrative_ontology:measurement_basis(hist_be_t2000, observed).
narrative_ontology:measurement(hist_be_t2024, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 2024, 0.89).
narrative_ontology:measurement_basis(hist_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1763, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1763, 0.78).
narrative_ontology:measurement_basis(hist_su_t1763, projected).
narrative_ontology:measurement(hist_su_t1830, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1830, 0.84).
narrative_ontology:measurement_basis(hist_su_t1830, observed).
narrative_ontology:measurement(hist_su_t1900, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1900, 0.88).
narrative_ontology:measurement_basis(hist_su_t1900, observed).
narrative_ontology:measurement(hist_su_t1950, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1950, 0.9).
narrative_ontology:measurement_basis(hist_su_t1950, observed).
narrative_ontology:measurement(hist_su_t2000, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 2000, 0.91).
narrative_ontology:measurement_basis(hist_su_t2000, observed).
narrative_ontology:measurement(hist_su_t2024, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 2024, 0.91).
narrative_ontology:measurement_basis(hist_su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1763, tn=2024
narrative_ontology:measurement(hist_grid_01, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(class), 1763, 0.89).
narrative_ontology:measurement(hist_grid_02, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(class), 2024, 0.95).
narrative_ontology:measurement(hist_grid_03, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(individual), 1763, 0.88).
narrative_ontology:measurement(hist_grid_04, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(individual), 2024, 0.94).
narrative_ontology:measurement(hist_grid_05, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(organizational), 1763, 0.92).
narrative_ontology:measurement(hist_grid_06, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(organizational), 2024, 0.96).
narrative_ontology:measurement(hist_grid_07, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(structural), 1763, 0.94).
narrative_ontology:measurement(hist_grid_08, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(structural), 2024, 0.97).
narrative_ontology:measurement(hist_grid_09, historical_treaty_substrate__extinguishment_reading, resistance(class), 1763, 0.64).
narrative_ontology:measurement(hist_grid_10, historical_treaty_substrate__extinguishment_reading, resistance(class), 2024, 0.72).
narrative_ontology:measurement(hist_grid_11, historical_treaty_substrate__extinguishment_reading, resistance(individual), 1763, 0.72).
narrative_ontology:measurement(hist_grid_12, historical_treaty_substrate__extinguishment_reading, resistance(individual), 2024, 0.62).
narrative_ontology:measurement(hist_grid_13, historical_treaty_substrate__extinguishment_reading, resistance(organizational), 1763, 0.68).
narrative_ontology:measurement(hist_grid_14, historical_treaty_substrate__extinguishment_reading, resistance(organizational), 2024, 0.68).
narrative_ontology:measurement(hist_grid_15, historical_treaty_substrate__extinguishment_reading, resistance(structural), 1763, 0.45).
narrative_ontology:measurement(hist_grid_16, historical_treaty_substrate__extinguishment_reading, resistance(structural), 2024, 0.51).
narrative_ontology:measurement(hist_grid_17, historical_treaty_substrate__extinguishment_reading, stakes_inflation(class), 1763, 0.84).
narrative_ontology:measurement(hist_grid_18, historical_treaty_substrate__extinguishment_reading, stakes_inflation(class), 2024, 0.92).
narrative_ontology:measurement(hist_grid_19, historical_treaty_substrate__extinguishment_reading, stakes_inflation(individual), 1763, 0.82).
narrative_ontology:measurement(hist_grid_20, historical_treaty_substrate__extinguishment_reading, stakes_inflation(individual), 2024, 0.91).
narrative_ontology:measurement(hist_grid_21, historical_treaty_substrate__extinguishment_reading, stakes_inflation(organizational), 1763, 0.86).
narrative_ontology:measurement(hist_grid_22, historical_treaty_substrate__extinguishment_reading, stakes_inflation(organizational), 2024, 0.93).
narrative_ontology:measurement(hist_grid_23, historical_treaty_substrate__extinguishment_reading, stakes_inflation(structural), 1763, 0.9).
narrative_ontology:measurement(hist_grid_24, historical_treaty_substrate__extinguishment_reading, stakes_inflation(structural), 2024, 0.96).
narrative_ontology:measurement(hist_grid_25, historical_treaty_substrate__extinguishment_reading, suppression(class), 1763, 0.78).
narrative_ontology:measurement(hist_grid_26, historical_treaty_substrate__extinguishment_reading, suppression(class), 2024, 0.9).
narrative_ontology:measurement(hist_grid_27, historical_treaty_substrate__extinguishment_reading, suppression(individual), 1763, 0.76).
narrative_ontology:measurement(hist_grid_28, historical_treaty_substrate__extinguishment_reading, suppression(individual), 2024, 0.88).
narrative_ontology:measurement(hist_grid_29, historical_treaty_substrate__extinguishment_reading, suppression(organizational), 1763, 0.8).
narrative_ontology:measurement(hist_grid_30, historical_treaty_substrate__extinguishment_reading, suppression(organizational), 2024, 0.92).
narrative_ontology:measurement(hist_grid_31, historical_treaty_substrate__extinguishment_reading, suppression(structural), 1763, 0.82).
narrative_ontology:measurement(hist_grid_32, historical_treaty_substrate__extinguishment_reading, suppression(structural), 2024, 0.93).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__extinguishment_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(historical_treaty_substrate__extinguishment_reading, 0.18).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate__nation_to_nation_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate__stewardship_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, reserve_system_legal_substrate).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, indigenous_federal_wardship_doctrine).

% DUAL FORMULATION NOTE:
% The historical_treaty_substrate kernel has THREE constraint readings: extinguishment_reading (this file), nation_to_nation_reading, and stewardship_reading. Each reading instantiates a different ε, different beneficiary/victim structure, and different classification. The extinguishment reading treats treaties as completed property transactions (snare); the nation_to_nation reading treats them as ongoing sovereign agreements (tangled_rope); the stewardship reading treats them as relational pacts (rope). These are not variants of one constraint—they are three structurally distinct constraints with incompatible axioms. The ε-invariance principle requires decomposition: one kernel, three readings, three separate constraint files, each with internal coherence. Network edges link the three readings as family members (affects_constraints cross-reference). Constraint families also include derivative constraints (reserve_system_legal_substrate and indigenous_federal_wardship_doctrine) that inherit from the primary reading that dominates institutional doctrine (extinguishment_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historical_treaty_substrate__extinguishment_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
