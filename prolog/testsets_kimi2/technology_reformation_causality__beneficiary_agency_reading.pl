% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__beneficiary_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__beneficiary_agency_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: technology_reformation_causality__beneficiary_agency_reading
 *   human_readable: Reformer-Printer Coalition for Authority Bypass
 *   domain: history/religious/media
 *
 * SUMMARY:
 *   This constraint instantiates the beneficiary_agency_reading of the
 *   technology_reformation_causality kernel. Under this reading, the
 *   Reformation was not caused by the printing press as an autonomous
 *   technological force, but rather by a strategic coalition of Protestant
 *   reformers and printing houses that deployed the press instrumentally to
 *   bypass Catholic Church authority over textual production and
 *   interpretation. The constraint is the reformer-printer coalition itself:
 *   an arrangement that coordinated heterodox theological production with
 *   commercial distribution while actively extracting authority from the
 *   ecclesiastical hierarchy. The coalition required active enforcement in
 *   the form of evasion networks, princely protection, and smuggling to
 *   persist against Church suppression. The 'technology as scaffold'
 *   interpretation treats the press as temporary support for a transition
 *   that would not have occurred without human strategic direction; once
 *   Protestant territories were established, the scaffold function mutated
 *   into establishment infrastructure.
 *
 * KEY AGENTS:
 *   - protestant_reformers: Agenda-setters and beneficiaries (organized/identity_locked) â directed print strategy and gained mass reach beyond episcopal control
 *   - printing_houses: Beneficiaries and risk-bearers (moderate/constrained) â provided capital, labor, and distribution for heterodox textual production
 *   - catholic_church_authority: Primary payer (institutional/trapped) â lost textual monopoly and interpretive control it had held for centuries
 *   - reformation_historians: Analytical observers assessing structural causality from outside the historical constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, 0.72).
domain_priors:suppression_score(technology_reformation_causality__beneficiary_agency_reading, 0.78).
domain_priors:theater_ratio(technology_reformation_causality__beneficiary_agency_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__beneficiary_agency_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__beneficiary_agency_reading, "Reformer-Printer Coalition for Authority Bypass").
narrative_ontology:topic_domain(technology_reformation_causality__beneficiary_agency_reading, "history/religious/media").

domain_priors:requires_active_enforcement(technology_reformation_causality__beneficiary_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__beneficiary_agency_reading, 'd940a420-c065-46e4-9729-e6a398dcab51').
narrative_ontology:cs_kernel_codification('d940a420-c065-46e4-9729-e6a398dcab51', distributed).
narrative_ontology:cs_authority_grounding('d940a420-c065-46e4-9729-e6a398dcab51', expertise).
narrative_ontology:cs_interpretation_layer_present('d940a420-c065-46e4-9729-e6a398dcab51').
narrative_ontology:cs_reading_relation('d940a420-c065-46e4-9729-e6a398dcab51', technology_reformation_causality__technological_determinism_reading, forecloses).
narrative_ontology:cs_reading_relation('d940a420-c065-46e4-9729-e6a398dcab51', technology_reformation_causality__co_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('d940a420-c065-46e4-9729-e6a398dcab51', foundational, human_agency_primacy_in_reformation).
narrative_ontology:cs_axiom_status(human_agency_primacy_in_reformation, holdable).
narrative_ontology:cs_axiom_grounding('d940a420-c065-46e4-9729-e6a398dcab51', human_agency_primacy_in_reformation, empirically_contingent).
narrative_ontology:cs_axiom('d940a420-c065-46e4-9729-e6a398dcab51', foundational, technology_as_neutral_instrument).
narrative_ontology:cs_axiom_status(technology_as_neutral_instrument, holdable).
narrative_ontology:cs_axiom_grounding('d940a420-c065-46e4-9729-e6a398dcab51', technology_as_neutral_instrument, empirically_contingent).
narrative_ontology:cs_reference_frame('d940a420-c065-46e4-9729-e6a398dcab51', reformer_agency_primacy).
narrative_ontology:cs_drift_state('d940a420-c065-46e4-9729-e6a398dcab51', post_empirical_turn, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d940a420-c065-46e4-9729-e6a398dcab51', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, protestant_reformers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, printing_houses).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, catholic_church_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, printing_houses).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Strategic theological leaders who chose which texts to print, shaped vernacular translation programs, and directed pamphlet campaigns to undermine Catholic episcopal censorship. They gained mass reach and bypassed Church control over interpretation, but became dependent on printer networks and vulnerable to political retaliation; recantation would constitute existential betrayal of their movement identity.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, protestant_reformers, agenda_setter,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, protestant_reformers, beneficiary).

% Commercial workshops that invested capital in presses, type, and paper to produce reformist texts. They profited from high-demand religious pamphlets and vernacular Bibles, but faced confiscation, excommunication, imprisonment, and mob violence from Catholic authorities and their allies.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, printing_houses, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, printing_houses, payer).

% The ecclesiastical hierarchy whose authority rested partly on controlling textual production, liturgical language, and interpretive monopoly. Lost the ability to suppress heterodox ideas at the source as vernacular print circulated beyond episcopal and inquisitorial control; could not exit its institutional identity even as its textual monopoly eroded.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, catholic_church_authority, payer,
    institutional, civilizational, trapped, continental).

% Modern scholars who debate whether the reformer-printer coalition demonstrates autonomous human agency or is better explained by technological determinism or co-constitution. They evaluate archival and bibliographic evidence but do not participate in the historical constraint.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, reformation_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__beneficiary_agency_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under conditions of religious censorship, the coalition solves the coordination problem between theological content producers and commercial distributors, enabling mass production of heterodox texts that neither reformers nor printers could achieve independently.
% TRANSFER_FUNCTION: Moves textual production control and interpretive authority from the Catholic Church to reformer-printer networks; moves revenue from readers to printers and religious allegiance from Catholic to Protestant institutions.
% ABSENT_VOICES: Catholic humanist printers who rejected both Church monopoly and radical reform; peasant radicals who used the same print networks for more destabilizing ends than reformers endorsed; women mystics and lay devotional authors whose textual authority was unrecognized by the male reformer-printer nexus.
% DISAPPEARANCE_RATIONALE: If the reformer-printer coalition vanished overnight in 1517, Luther's ideas would have remained scribal curiosities confined to academic disputations; the rapid European diffusion of Protestant theology would not have occurred, and the Church would have retained its textual monopoly for decades longer.
% FOUNDING_PROBLEM: How to disseminate theological critiques and vernacular scripture outside Church-controlled scribal and episcopal channels.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Catholic polemicists (e.g., Johannes Cochlaeus) attested the threat of uncontrolled print. Modern book historians (Eisenstein, Johns, Pettegree) corroborate that the problem of textual bypass was substantially solved once Protestant territorial churches established legal protections for reformist printing. Corroboration comes from outside the beneficiary coalition.
narrative_ontology:disappearance_verdict(technology_reformation_causality__beneficiary_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__beneficiary_agency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__beneficiary_agency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_reformation_causality__beneficiary_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__beneficiary_agency_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__beneficiary_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_reformation_causality__beneficiary_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the coalition transferred substantial textual and interpretive authority from the Church to reformer-printer networks; this is authority bypass, not merely market exchange. Suppression (0.78) reflects the active enforcement required â both the Church's censorship attempts and the coalition's evasion machinery â and the collapse of the Church's scribal alternative. Theater ratio (0.30) is moderate: the coordination was largely functional (real texts, real distribution), but public disputations, propaganda campaigns, and symbolic Bible burnings added performative dimensions. Accessibility collapse (0.68) measures how completely the Church's scribal alternative became non-viable once the print coalition scaled. Resistance (0.82) is very high because the Church mounted sustained counter-measures (Index of Forbidden Books, inquisitorial pursuit, political pressure at Imperial diets). The temporal series show extraction rising through the 1520s, peaking as the coalition institutionalized, then slightly normalizing as the arrangement became establishment rather than bypass.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer and printer seats, the constraint is necessary coordination solving a real problem (how to disseminate ideas under censorship) and legitimate resistance to tyrannical control. From the Church seat, the same structure is aggressive extraction of its constitutive authority, heretical subversion, and asymmetric warfare against a millennium-old textual order. The engine computes this divergence from the structural role and exit data: agenda-setter/beneficiary with identity-locked exit versus institutional payer with trapped exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Protestant reformers are low-d beneficiaries: they set the agenda, gained mass audiences, and had identity-locked exit (recantation was existential betrayal). Printing houses are low-moderate-d beneficiaries: they profited and were protected, but bore persecution risk. Catholic Church authority is high-d payer: it bore the full cost of lost monopoly and had no exit from its institutional identity. Reformation historians are analytical (analytical exit, no directionality). No overrides are needed; the structural derivation captures the historical relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â bypassing Church censorship to disseminate heterodox theology â was substantially solved once Protestant princes established territorial churches and legal protections for reformist printing. The coalition did not atrophy into pure performance (piton), nor did it become pure extraction (snare), because it transformed into legitimate establishment infrastructure. This is a tangled rope that succeeded its founding transition: the R5 status 'dead' marks mission completion, not dysfunction. The classification prevents mislabeling the later establishment phase as either ongoing coordination or as inertial theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_divergence,
    'How would the constraint''s classification change under the technological_determinism or co_constitution sibling readings?',
    'Comparative analysis of the three generated constraint stories in this kernel family.',
    'Technological determinism would likely classify the press itself as a mountain or self-enforcing constraint with minimal human agency; co-constitution would distribute directionality more evenly across social and technical actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_divergence, conceptual, 'Committer-frame uncertainty about sibling reading structural deltas').

omega_variable(
    reformer_printer_asymmetry,
    'Does the reformer-printer coalition involve mutual extraction between partners, or is extraction asymmetrically borne by Catholic Church authority?',
    'Archival economic analysis of printer profit margins versus reformer political gains, and whether either party could exit without catastrophic loss.',
    'If mutual, the coalition is a tighter tangled rope; if asymmetric, one partner is a concealed payer and the structure approaches snare for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformer_printer_asymmetry, empirical, 'Internal coalition extraction asymmetry').

omega_variable(
    scaffold_transition_or_mandatrophy,
    'Did the printing coalition transition legitimately from scaffold to establishment, or did it undergo mandatrophy by persisting after its bypass function became obsolete?',
    'Evaluate whether post-establishment Protestant printing maintained the same structural relationships (agenda_setter, payer, beneficiary) as the bypass coalition, or reorganized into a different constraint type.',
    'If mandatrophy, the later phase is piton; if legitimate transformation, it is a distinct constraint (possibly rope or scaffold completion).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaffold_transition_or_mandatrophy, conceptual, 'Post-founder transition ambiguity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__beneficiary_agency_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(tech_tr_t5, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(tech_tr_t10, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(tech_tr_t15, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(tech_tr_t20, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(tech_tr_t25, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(tech_tr_t30, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tech_be_t5, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(tech_be_t10, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(tech_be_t15, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(tech_be_t20, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(tech_be_t25, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 25, 0.72).
narrative_ontology:measurement(tech_be_t30, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(tech_su_t5, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(tech_su_t10, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(tech_su_t15, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 15, 0.8).
narrative_ontology:measurement(tech_su_t20, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 20, 0.82).
narrative_ontology:measurement(tech_su_t25, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 25, 0.8).
narrative_ontology:measurement(tech_su_t30, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__beneficiary_agency_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__co_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the technology_reformation_causality kernel family. The kernel decomposes into three structurally distinct readings because the epsilon values, beneficiary structures, and directionality profiles differ across causal attributions (agency vs. determinism vs. co-evolution). Each reading is compiled as a separate constraint story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
