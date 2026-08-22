% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__adaptive_fiction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__adaptive_fiction_reading, []).

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
 *   constraint_id: lycurgan_laws__adaptive_fiction_reading
 *   human_readable: Lycurgan Immutability as Noble Lie Masking Covert Adaptation
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   This story instantiates the adaptive-fiction reading of the Lycurgan
 *   kernel: the claim that Sparta's law was immutable, divinely sanctioned,
 *   and unchanged since its legendary founder is read as a noble lie that
 *   concealed continuous, elite-controlled institutional adaptation. On this
 *   reading the gerousia, ephorate, and dual kingship quietly reinterpreted
 *   land tenure, military obligation, citizenship qualification, and helot
 *   management across centuries while maintaining the public fiction of
 *   changelessness — because the fiction itself performed real coordination
 *   work (foreclosing factional relitigation, signaling stability to rivals)
 *   even as it let the same elite capture the benefits of adaptation without
 *   political accountability for having adapted. The demographic collapse
 *   (oliganthropia) on this reading is not caused by rigidity itself but by
 *   the enforcement gap the fiction created: land-tenure rules eroded
 *   informally exactly where formal, acknowledged reform would have preserved
 *   the citizen base. This is a genuinely different structural claim from the
 *   sacral_fidelity_reading (which holds the laws WERE followed as sacred,
 *   unchangeable ordinance) and the demographic_trap_reading (which holds the
 *   laws were rigid and it was the rigidity itself, not covert circumvention,
 *   that caused collapse) — the three readings assign different causal
 *   mechanisms to the same historical outcome and cannot be collapsed into
 *   one ε.
 *
 * KEY AGENTS:
 *   - spartan_gerousia: agenda_setter (institutional/arbitrage) — controls interpretive drift while claiming continuity
 *   - ephorate: agenda_setter/beneficiary (institutional/arbitrage) — annual magistrates who launder adaptation as compliance
 *   - dual_kingship: beneficiary/agenda_setter (institutional/constrained) — legitimated by the fiction, negotiates real change beneath it
 *   - spartiate_citizens_facing_oliganthropia: payer (moderate/trapped) — loses land and citizenship status to unacknowledged drift
 *   - helot_population: payer (powerless/trapped) — bears the cost of periodically intensified control dressed as ancient custom
 *   - perioikoi_communities: payer (powerless/constrained) — absorbs rising military burden from unadmitted citizen shortage
 *   - delphic_oracle: beneficiary (organized/arbitrage) — supplies retroactive sanction, gaining authority from each invocation
 *   - later_political_theorists: observer (analytical/analytical) — reconstructs the gap between professed and actual constitutional practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, 0.58).
domain_priors:suppression_score(lycurgan_laws__adaptive_fiction_reading, 0.62).
domain_priors:theater_ratio(lycurgan_laws__adaptive_fiction_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(lycurgan_laws__adaptive_fiction_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__adaptive_fiction_reading, tangled_rope).
narrative_ontology:human_readable(lycurgan_laws__adaptive_fiction_reading, "Lycurgan Immutability as Noble Lie Masking Covert Adaptation").
narrative_ontology:topic_domain(lycurgan_laws__adaptive_fiction_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(lycurgan_laws__adaptive_fiction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__adaptive_fiction_reading, '04593952-c7c4-4799-924b-b18b8d3da9b0').
narrative_ontology:cs_kernel_codification('04593952-c7c4-4799-924b-b18b8d3da9b0', fixed_text).
narrative_ontology:cs_authority_grounding('04593952-c7c4-4799-924b-b18b8d3da9b0', lineage).
narrative_ontology:cs_interpretation_layer_present('04593952-c7c4-4799-924b-b18b8d3da9b0').
narrative_ontology:cs_reading_relation('04593952-c7c4-4799-924b-b18b8d3da9b0', lycurgan_laws__sacral_fidelity_reading, forecloses).
narrative_ontology:cs_reading_relation('04593952-c7c4-4799-924b-b18b8d3da9b0', lycurgan_laws__demographic_trap_reading, influences).
narrative_ontology:cs_axiom('04593952-c7c4-4799-924b-b18b8d3da9b0', foundational, constitutional_fixity_is_performative_not_descriptive).
narrative_ontology:cs_axiom_status(constitutional_fixity_is_performative_not_descriptive, holdable).
narrative_ontology:cs_axiom_grounding('04593952-c7c4-4799-924b-b18b8d3da9b0', constitutional_fixity_is_performative_not_descriptive, empirically_contingent).
narrative_ontology:cs_axiom('04593952-c7c4-4799-924b-b18b8d3da9b0', secondary, interpretive_monopoly_enables_covert_reform).
narrative_ontology:cs_axiom_status(interpretive_monopoly_enables_covert_reform, holdable).
narrative_ontology:cs_axiom_grounding('04593952-c7c4-4799-924b-b18b8d3da9b0', interpretive_monopoly_enables_covert_reform, empirically_contingent).
narrative_ontology:cs_reference_frame('04593952-c7c4-4799-924b-b18b8d3da9b0', lycurgan_founding_settlement).
narrative_ontology:cs_drift_state('04593952-c7c4-4799-924b-b18b8d3da9b0', late_classical_oliganthropia_period, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('04593952-c7c4-4799-924b-b18b8d3da9b0', '').
narrative_ontology:cs_kernel_id(lycurgan_laws__adaptive_fiction_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, spartan_gerousia).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, ephorate).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, dual_kingship).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, perioikoi_communities).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, helot_population).
narrative_ontology:constraint_victim(lycurgan_laws__adaptive_fiction_reading, spartiate_citizens_facing_oliganthropia).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lycurgan_laws__adaptive_fiction_reading, delphic_oracle).
narrative_ontology:constraint_vindicates(lycurgan_laws__adaptive_fiction_reading, rhetra_divine_origin_doctrine).
narrative_ontology:constraint_vindicates(lycurgan_laws__adaptive_fiction_reading, eunomia_as_fixed_constitutional_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Council of elders holds interpretive control over what counts as consistent with the Great Rhetra. Quietly ratifies reinterpretations of land tenure, military obligation, and citizenship rules while publicly maintaining that the laws of Lycurgus are untouched since their founding. Its members are drawn from the same families that benefit from each successive reinterpretation.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartan_gerousia, agenda_setter,
    institutional, generational, arbitrage, national).

% Annually elected magistrates who enforce compliance with 'ancestral' custom while functionally serving as the mechanism through which adaptation is smuggled in — reinterpreting oracular sanction, adjusting enforcement of the agoge, and modulating land-transfer rules under cover of continuity. Their short terms let each board disclaim responsibility for the drift the office as a whole produces.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, ephorate, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, ephorate, beneficiary).

% The two royal houses invoke Lycurgan sanctity to legitimate their hereditary privilege, while simultaneously negotiating real changes to military command, land grants to favored commanders, and marriage practices that circumvent the same laws they claim to uphold unchanged.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, dual_kingship, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__adaptive_fiction_reading, dual_kingship, agenda_setter).

% Full citizens whose land allotments (kleroi) are supposed to be inalienable and equal under Lycurgan law, but who watch estates concentrate in fewer hands as the elite class quietly permits inheritance and dowry practices the 'unchangeable' law was said to forbid. Losing the land qualification means losing citizenship itself — exit from Sparta means losing identity, but staying means watching the rule erode under them without any official acknowledgment that it has changed.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, spartiate_citizens_facing_oliganthropia, payer,
    moderate, biographical, trapped, national).

% State-owned agricultural serfs bound to the land under the same constitutional order that claims permanence. The annual ritual declaration of war on the helots (part of the supposedly fixed ephoral custom) is itself an adaptive tool — a mechanism periodically intensified or relaxed as the elite manages labor unrest, dressed as ancient unalterable practice.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, helot_population, payer,
    powerless, generational, trapped, regional).

% Free but non-citizen dependent townspeople whose military and economic obligations to Sparta shift as needed while framed as fixed ancestral arrangements. They bear escalating military burden as Spartiate numbers decline, absorbing the cost of a citizen shortage the official story does not admit exists.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, perioikoi_communities, payer,
    powerless, generational, constrained, regional).

% Provides retroactive divine sanction for whatever reinterpretation the Spartan authorities require, including the original Rhetra itself. Its authority is enhanced each time its pronouncements are invoked to bless a change dressed as continuity.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, delphic_oracle, beneficiary,
    organized, civilizational, arbitrage, continental).

% Plutarch, Aristotle, and modern constitutional theorists examine the gap between Sparta's professed changelessness and its documented institutional drift, using the case to theorize how constitutional fictions function as governance technology rather than as descriptively accurate claims.
narrative_ontology:constraint_stakeholder(lycurgan_laws__adaptive_fiction_reading, later_political_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__adaptive_fiction_reading, diffuse).
narrative_ontology:fixing_cost_class(lycurgan_laws__adaptive_fiction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The claim of unchangeable, divinely-sanctioned law solves a genuine coordination problem: it provides a stable Schelling point against factional relitigation of the constitutional order, reduces costly political conflict over redistribution, and gives external allies and rivals a credible signal of Spartan institutional continuity.
% TRANSFER_FUNCTION: The immutability fiction moves flexibility-value from those who would benefit from acknowledged, contestable reform (smallholding citizens, helots, perioikoi seeking formal renegotiation of their burdens) to those positioned to quietly control interpretation (gerousia, ephors, kings) — who capture the benefits of adaptation without paying the political cost of admitting the system changed.
% ABSENT_VOICES: Helots and perioikoi have no interpretive standing at all — they cannot invoke the Rhetra's supposed fixity to protect themselves from the very adaptations that increase their burdens, because the interpretive machinery is monopolized by those it benefits. Ordinary Spartiates watching their kleros erode have no forum to contest the informal changes precisely because officially nothing has changed.
% DISAPPEARANCE_RATIONALE: If the immutability fiction were exposed and abandoned overnight, the Spartan elite would lose the primary legitimating device that let it adjust rules without triggering demands for formal renegotiation from every affected party; land concentration, helot management, and citizenship qualification would all become explicitly contestable political questions rather than matters of 'ancestral law,' likely accelerating exactly the instability the fiction was designed to prevent.
% FOUNDING_PROBLEM: Archaic Sparta needed to end recurring civil conflict (stasis) over land redistribution and constitutional authority; a founding myth of a single lawgiver (Lycurgus) delivering a complete, oracle-sanctioned, permanent order took the constitution out of ordinary political contest.
% FOUNDING_PROBLEM_CORROBORATION: Aristotle's Politics and later historians attest that Spartan practice diverged substantially from the professed Lycurgan constitution well before the classical period, and that oliganthropia (citizen population collapse) proceeded alongside — not despite — significant informal adaptation in land tenure and military policy; this corroboration comes from observers analyzing the outcome, not from the gerousia or ephorate who benefited from maintaining the fiction and never conceded the adaptation was occurring.
narrative_ontology:disappearance_verdict(lycurgan_laws__adaptive_fiction_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__adaptive_fiction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__adaptive_fiction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lycurgan_laws__adaptive_fiction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__adaptive_fiction_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__adaptive_fiction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lycurgan_laws__adaptive_fiction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lycurgan_laws__adaptive_fiction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.58 — substantial but not maximal — because the fiction genuinely performs a coordination function (reducing costly stasis) even as it channels the gains from adaptation disproportionately to the interpreting elite. Theater ratio is authored high and rising (0.40 to 0.71) because on this reading an increasing share of Spartan civic and religious performance (rhetra recitation, agoge ritual, oracular consultation) exists specifically to sustain the appearance of unbroken continuity precisely as the substance diverges further from it — the theater intensifies as the gap it must paper over widens. Suppression is moderate (0.62): the mechanism does not rely primarily on coercive force but on monopolized interpretive authority and the absence of any legitimate forum to name a change as a change.
 *
 * DIRECTIONALITY LOGIC:
 *   The gerousia, ephorate, and kings sit near the beneficiary end: they control the interpretive apparatus and capture adaptation's gains (favorable land arrangements, retained privilege) without bearing the political cost of proposing reform openly. Spartiate citizens facing land erosion, helots, and perioikoi sit near the target end: they bear the costs of adaptations they had no voice in authorizing and cannot contest because those adaptations are never officially acknowledged as changes. The delphic oracle is a curious secondary beneficiary — an external, immobile-seeming institution whose authority is actually reinforced, not spent, by each retroactive sanction it grants, giving it real arbitrage-grade positioning despite apparent detachment from Spartan domestic politics.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification here specifically prevents mislabeling this as a pure snare (which would deny the coordination function ever existed) or as a pure mountain (which is what the sacral_fidelity_reading claims and what the noble lie was designed to make outsiders believe). The genuine coordination benefit — avoiding recurrent stasis over land and constitutional authority — is real and shared broadly at the founding; the extraction is what happens as the same mechanism is captured over generations by those positioned to interpret it. The founding_problem is authored as contested rather than dead because on this reading the coordination function partially persists (Sparta did avoid certain forms of civil war other poleis suffered) even as its administration became increasingly self-serving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiction_vs_genuine_belief,
    'Did the Spartan elite (gerousia, ephors, kings) consciously understand the immutability claim as a strategic fiction, or did they themselves believe in Lycurgan permanence even while implementing changes they did not perceive as changes?',
    'Comparative analysis of contemporaneous Spartan self-description versus external observer accounts (Herodotus, Thucydides, Xenophon) for moments where insiders explicitly acknowledge adaptation versus moments where drift appears to have been genuinely unrecognized as drift by participants.',
    'If elites genuinely believed the fiction rather than strategically deploying it, this reading''s ''noble lie'' framing overstates conscious agency — the extraction would be better modeled as unintentional institutional capture rather than deliberate manipulation, which would lower confidence in requires_active_enforcement as currently authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiction_vs_genuine_belief, conceptual, 'Whether the immutability claim was a knowing strategic fiction or a genuinely-held false belief among the interpreting elite.').

omega_variable(
    coordination_extraction_ratio_over_time,
    'At what point, if any, did the balance between the fiction''s genuine coordination benefit (avoiding stasis) and its extractive capture by the interpreting elite tip decisively toward extraction?',
    'Longitudinal reconstruction of land distribution records, citizen-count trends, and documented instances of ephoral/royal rule reinterpretation across the classical period, correlated against periods of external military pressure (when the coordination benefit would be most valuable) versus periods of internal stability (when interpretive capture would be least constrained).',
    'If the tip point can be dated, the story could be split into two temporal phases with different classifications (rope-dominant early, tangled_rope/snare-dominant late) rather than a single tangled_rope classification averaged across four centuries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_ratio_over_time, empirical, 'Whether the coordination-to-extraction balance shifted at an identifiable historical point rather than holding steady.').

omega_variable(
    sibling_reading_evidentiary_overlap,
    'How much of the primary evidentiary record (Plutarch''s Lycurgus, Aristotle''s Politics Book II) is actually decisive between this reading and the demographic_trap_reading, versus how much is genuinely underdetermined between ''rigid system that broke'' and ''flexible system whose flexibility was denied''?',
    'Systematic review of which specific claims in the ancient sources support adaptation-with-denial versus genuine-rigidity, distinguishing textual evidence from later historiographic inference.',
    'If the evidentiary record substantially underdetermines the choice between this reading and demographic_trap_reading, both readings should carry higher uncertainty in their ε values and the network relationship between them should register as evidentiary tension rather than clean structural distinction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_evidentiary_overlap, conceptual, 'Whether the two ''why did Sparta decline'' readings are evidentiarily distinguishable or reflect an interpretive choice underdetermined by the sources.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__adaptive_fiction_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycu_tr_t0, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(lycu_tr_t80, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 80, 0.48).
narrative_ontology:measurement(lycu_tr_t160, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 160, 0.56).
narrative_ontology:measurement(lycu_tr_t240, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 240, 0.63).
narrative_ontology:measurement(lycu_tr_t320, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 320, 0.68).
narrative_ontology:measurement(lycu_tr_t400, lycurgan_laws__adaptive_fiction_reading, theater_ratio, 400, 0.71).

% Extraction over time
narrative_ontology:measurement(lycu_be_t0, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(lycu_be_t80, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(lycu_be_t160, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 160, 0.48).
narrative_ontology:measurement(lycu_be_t240, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 240, 0.53).
narrative_ontology:measurement(lycu_be_t320, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 320, 0.56).
narrative_ontology:measurement(lycu_be_t400, lycurgan_laws__adaptive_fiction_reading, base_extractiveness, 400, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lycu_su_t0, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(lycu_su_t80, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 80, 0.54).
narrative_ontology:measurement(lycu_su_t160, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 160, 0.58).
narrative_ontology:measurement(lycu_su_t240, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 240, 0.6).
narrative_ontology:measurement(lycu_su_t320, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 320, 0.61).
narrative_ontology:measurement(lycu_su_t400, lycurgan_laws__adaptive_fiction_reading, suppression_requirement, 400, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__adaptive_fiction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lycurgan_laws__adaptive_fiction_reading, 0.12).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, sacral_fidelity_reading).
narrative_ontology:affects_constraint(lycurgan_laws__adaptive_fiction_reading, demographic_trap_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the single natural-language label 'Lycurgan immutability': sacral_fidelity_reading (mountain — the laws were genuinely followed as fixed divine ordinance), demographic_trap_reading (a rigidity-caused-collapse reading treating the fixed system itself, taken at face value, as the mechanism of demographic failure), and this adaptive_fiction_reading (tangled_rope — the professed fixity concealed continuous elite-controlled adaptation, and demographic failure resulted from the enforcement gap the covert adaptation created, not from rigidity per se). Each reading assigns a different ε and a different causal architecture to the same historical outcome; they are linked via affects_constraints rather than merged, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
