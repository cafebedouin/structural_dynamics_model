% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__classical_abrogation, []).

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
 *   constraint_id: naskh_principle__classical_abrogation
 *   human_readable: Classical Doctrine of Naskh (Chronological Abrogation)
 *   domain: religious/legal/hermeneutical
 *
 * SUMMARY:
 *   This story instantiates the classical abrogation (naskh) reading of the
 *   Quranic-supersession kernel: when two verses address the same legal or
 *   theological topic and appear to conflict, the verse revealed later in
 *   chronological order legally supersedes the earlier one, which is retained
 *   in the text for recitation and spiritual instruction but loses binding
 *   legal force. This is a coordination mechanism for legal determinacy that
 *   hardened, over roughly a millennium of juristic elaboration, into an
 *   apparatus whose scale (competing abrogation lists ranging from a handful
 *   to hundreds of verses) is itself now contested even within the tradition
 *   that produced it. The metrics describe the classical reading's own
 *   operation as this reading's proponents and its internal critics would
 *   describe it — not the endorsed alternative harmonizing or progressive
 *   readings, which are separate constraints.
 *
 * KEY AGENTS:
 *   - classical_fiqh_schools: institutional agenda-setter and beneficiary — administers the abrogation corpus
 *   - state_backed_judiciaries: institutional beneficiary — applies settled rulings without re-litigating theology
 *   - abrogation_specialist_scholars: organized, identity-locked beneficiary — professional authority constituted by mastery of the apparatus
 *   - harmonization_minded_jurists: moderate-power payer — textual arguments discounted by institutional precedent
 *   - lay_readers_of_scripture: powerless, trapped payer — must defer to the administered hierarchy
 *   - reformist_theologians: moderate-power, generational payer — blocked from grounding reform in verses the hierarchy voids
 *   - comparative_hermeneutics_scholars: analytical observer — traces the doctrine's own historical contestation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.52).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.58).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.52).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Doctrine of Naskh (Chronological Abrogation)").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "religious/legal/hermeneutical").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, 'c3427fda-2aa7-4bb7-b7dd-9fc30f9e2251').
narrative_ontology:cs_kernel_codification('c3427fda-2aa7-4bb7-b7dd-9fc30f9e2251', formalized).
narrative_ontology:cs_authority_grounding('c3427fda-2aa7-4bb7-b7dd-9fc30f9e2251', lineage).
narrative_ontology:cs_interpretation_layer_present('c3427fda-2aa7-4bb7-b7dd-9fc30f9e2251').
narrative_ontology:cs_reading_relation('c3427fda-2aa7-4bb7-b7dd-9fc30f9e2251', naskh_principle__contextual_harmonization, forecloses).
narrative_ontology:cs_reading_relation('c3427fda-2aa7-4bb7-b7dd-9fc30f9e2251', naskh_principle__progressive_restriction, coexists_with).
narrative_ontology:cs_axiom('c3427fda-2aa7-4bb7-b7dd-9fc30f9e2251', foundational, chronological_priority_determines_legal_validity).
narrative_ontology:cs_axiom_status(chronological_priority_determines_legal_validity, holdable).
narrative_ontology:cs_axiom_grounding('c3427fda-2aa7-4bb7-b7dd-9fc30f9e2251', chronological_priority_determines_legal_validity, conventional).
narrative_ontology:cs_axiom('c3427fda-2aa7-4bb7-b7dd-9fc30f9e2251', secondary, abrogated_verses_retain_only_recitational_not_legal_force).
narrative_ontology:cs_axiom_status(abrogated_verses_retain_only_recitational_not_legal_force, holdable).
narrative_ontology:cs_axiom_grounding('c3427fda-2aa7-4bb7-b7dd-9fc30f9e2251', abrogated_verses_retain_only_recitational_not_legal_force, conventional).
narrative_ontology:cs_reference_frame('c3427fda-2aa7-4bb7-b7dd-9fc30f9e2251', early_juristic_consolidation_consensus).
narrative_ontology:cs_drift_state('c3427fda-2aa7-4bb7-b7dd-9fc30f9e2251', contemporary_reformist_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c3427fda-2aa7-4bb7-b7dd-9fc30f9e2251', '').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, classical_fiqh_schools).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, state_backed_judiciaries).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, abrogation_specialist_scholars).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, harmonization_minded_jurists).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, lay_readers_of_scripture).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, reformist_theologians).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, chronological_supersession_doctrine).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, asbab_al_nuzul_priority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and teach the canonical abrogation lists (nasikh wa mansukh literature) that determine which verses control legal rulings. Their fatwas and school doctrines rest on settled supersession chains; they train the jurists who apply them and control the curricula that reproduce the doctrine.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, classical_fiqh_schools, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__classical_abrogation, classical_fiqh_schools, beneficiary).

% Apply codified personal-status and criminal law derived from abrogation-settled rulings (e.g., later verses on inheritance, warfare, alcohol). A fixed hierarchy of verses gives courts a determinate rule to apply without re-litigating theology in every case; predictability is the operational payoff.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, state_backed_judiciaries, beneficiary,
    institutional, generational, arbitrage, national).

% Build scholarly careers, authority, and institutional standing on expertise in the abrogation corpus (identifying which of the disputed 5-500+ claimed abrogated verses are sound). Their professional identity and standing are constituted by mastery of this apparatus; abandoning the doctrine would dissolve their specialized authority.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, abrogation_specialist_scholars, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__classical_abrogation, abrogation_specialist_scholars, agenda_setter).

% Argue that many claimed abrogations are hadith-dependent and juristically unsettled, and that contextual reading would preserve legal force in verses the classical schema discards. Publishing or ruling against the abrogation hierarchy risks being labeled outside the tradition, so their interpretive options are narrowed even where their textual arguments are strong.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, harmonization_minded_jurists, payer,
    moderate, biographical, constrained, national).

% Encounter the text directly and see apparent contradictions the abrogation apparatus resolves for them by declaring one verse legally dead. They generally lack the specialist training to contest the abrogation list themselves and must accept the received hierarchy or defer entirely to scholars who administer it.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, lay_readers_of_scripture, payer,
    powerless, biographical, trapped, global).

% Seek to read the whole revealed corpus as coherently binding (via contextual or progressive readings) to ground reform arguments (e.g., on gender or punishment law) in verses the classical hierarchy treats as abrogated and legally inert. The abrogation apparatus is precisely the mechanism that blocks their preferred verses from having legal standing today.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, reformist_theologians, payer,
    moderate, generational, constrained, global).

% Study how the abrogation doctrine emerged historically (2nd-3rd century AH juristic consolidation), compare it to other scriptural supersession doctrines, and document how the size of the claimed-abrogation list has itself been contested and shrunk over centuries by scholars working from within the tradition.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, comparative_hermeneutics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__classical_abrogation, diffuse).
narrative_ontology:fixing_cost_class(naskh_principle__classical_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate procedure for resolving apparent legal contradictions within the Quranic corpus: when two verses conflict on the same topic, the later-revealed verse controls, giving judges, muftis, and legislators a single settled rule rather than an open-ended interpretive dispute in every case.
% TRANSFER_FUNCTION: Moves interpretive authority and legal certainty toward institutions that administer the abrogation corpus (schools, specialist scholars, state judiciaries) and away from readers, reform movements, and jurists who would ground legal claims in verses the hierarchy declares superseded — those parties lose access to textual support that would otherwise be available under a harmonizing reading.
% ABSENT_VOICES: Contextual-harmonization and progressive-restriction readers are structurally sidelined within courts and curricula that have institutionalized the classical abrogation lists as settled; their arguments are treated as minority or reformist positions rather than live jurisprudential alternatives, even where their textual reasoning is at least as old.
% DISAPPEARANCE_RATIONALE: If the classical abrogation hierarchy were abandoned overnight, personal-status codes, war and punishment jurisprudence, and centuries of fatwa precedent built on 'later verse controls' rulings would lose their textual anchor; courts would need to re-derive rulings through contextual or progressive readings, and the specialist authority of abrogation scholars would collapse into a historical rather than a live legal discipline.
% FOUNDING_PROBLEM: Early Muslim jurists confronted verses on the same topic (alcohol, qibla direction, warfare, inheritance) that appeared to give different or conflicting rulings across the twenty-three years of revelation, and needed a workable procedure to apply Quranic law consistently rather than leaving every ruling to case-by-case theological dispute.
% FOUNDING_PROBLEM_CORROBORATION: Classical fiqh schools and abrogation specialists attest the problem remains live because unresolved apparent contradictions still require a determinate rule. Independent historians of Islamic law and some Sunni and Shi'i revisionist jurists — working outside the beneficiary institutions — attest that the scale of the abrogation lists (claims ranging from 5 to over 500 verses across different classical compilers) was itself a product of later juristic consolidation rather than a stable early consensus, and that many claimed abrogations are now treated as spurious even within traditional scholarship, suggesting the mechanism outran the problem it was built to solve.
narrative_ontology:disappearance_verdict(naskh_principle__classical_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__classical_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__classical_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(naskh_principle__classical_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__classical_abrogation, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__classical_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__classical_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__classical_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) rather than high: the coordination function (resolving genuine apparent contradiction for legal application) is real and not merely cover, but the doctrine also concentrates interpretive authority and forecloses textually defensible alternative readings, which is the extraction component. Suppression (0.58) exceeds extraction because the mechanism by which alternative readings are foreclosed is largely institutional and educational rather than physically coercive — accreditation, curricular gatekeeping, and precedent — but it is nonetheless an active, enforced narrowing of what counts as a legitimate ruling. Theater ratio is modest-low (0.28): most of the apparatus still does real interpretive work, though the historically inflated and later-pruned abrogation lists (500+ claimed instances shrinking to a much smaller scholarly consensus) show some accumulated performative excess. All three metrics share one time grid across the doctrine's roughly 1400-year history, with the classical era (0-200 AH equivalent, mapped here to years 0-200) showing lower suppression as the doctrine was still consolidating, rising toward the observed present-day plateau.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (classical fiqh schools, state judiciaries), the doctrine is straightforward coordination: a determinate procedure applied consistently for centuries. From the payer seats (harmonization-minded jurists, reformist theologians, lay readers), the same structure operates as an enforced narrowing that forecloses textually live alternatives and channels legal authority toward institutions that administer the supersession lists. The engine computes this divergence from the structural directionality data; the claimed_type of tangled_rope reflects the authoring judgment that both the coordination function and the asymmetric extraction are genuinely present — this is not a pure snare because the underlying problem (resolving apparent contradiction) is real, and not a pure rope because the resolution mechanism concentrates authority and forecloses live alternatives rather than merely coordinating among equals.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical fiqh schools, state judiciaries, and abrogation specialists are declared beneficiaries: they collect authority, legal predictability, or professional standing from the doctrine's operation and have institutional or arbitrage-grade exit (they administer the framework rather than being subject to it). Harmonization-minded jurists, lay readers, and reformist theologians are declared victims: they bear the cost of foreclosed textual readings, with exit options ranging from constrained (jurists who risk being read out of the tradition) to trapped (lay readers with no specialist standing to contest the received hierarchy). Abrogation specialist scholars carry identity_locked exit deliberately overridden toward the target end in spirit even though nominally a beneficiary group, because their professional identity is constituted by the doctrine's continued authority — this is documented as an identity-lock dynamic rather than a pure override, since the beneficiary declaration already captures the primary direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (resolving apparent textual contradiction for consistent legal application) remains genuinely live in some form — courts and scholars still need a procedure for handling apparently conflicting verses. But the specific classical apparatus, with its historically inflated and internally contested abrogation lists, shows signs of having outrun the narrower problem it solved: many classical compilers' abrogation counts are now treated as overstated even by scholars within the tradition, suggesting institutional inertia and professional identity investment (abrogation_specialist_scholars) sustain a larger and more rigid supersession hierarchy than the live problem strictly requires. This is not classified as pure mandatrophy (function fully dead) because state judiciaries still apply abrogation-derived rulings as live law — the founding_problem_status of 'contested' reflects this genuine split between live application and internally documented overreach.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abrogation_count_authenticity,
    'How many of the classically claimed abrogation instances (estimates range from roughly 5 to over 500 across different compilers) rest on sound chains of transmission versus later juristic inference or convenience?',
    'Isnad-critical and philological review of each claimed abrogation pair against the broader corpus of accepted hadith and reported occasions of revelation (asbab al-nuzul), cross-checked against modern text-critical scholarship on dating verses.',
    'A finding that most claimed abrogations are unsound would sharply shrink the doctrine''s legitimate scope and support the harmonization or progressive-restriction readings for those specific verse pairs, while a finding that a core set is well-attested would preserve classical abrogation as structurally sound for that subset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_count_authenticity, empirical, 'Whether the size of the classical abrogation corpus reflects sound transmission or later inflation.').

omega_variable(
    kernel_reading_selection_criterion,
    'Is chronological supersession the correct default reading of apparent Quranic contradiction, or is it one contestable hermeneutic choice among the three live readings (classical abrogation, contextual harmonization, progressive restriction) with no neutral tiebreaker internal to the text itself?',
    'Comparative analysis of which reading each classical school selected historically and why, and whether any argument for chronological supersession as default is textually compelled versus theologically or institutionally motivated (e.g., need for legal determinacy in early Islamic state-building).',
    'If chronological supersession is textually underdetermined relative to the sibling readings, the classical reading''s claim to represent ''the'' correct interpretation weakens, and its extractiveness score (concentration of interpretive authority) should be read as partly a function of institutional consolidation rather than textual necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_criterion, conceptual, 'Whether classical abrogation is the textually compelled reading or one institutionally favored option among defensible alternatives.').

omega_variable(
    legal_certainty_versus_theological_coherence_tradeoff,
    'Is the tradeoff this reading makes (fixed legal rulings and certainty, at the cost of interpretive flexibility and apparent theological coherence) a tradeoff the tradition itself would endorse if the scale of contested abrogations were made fully transparent to lay adherents?',
    'Survey or historical-sociological study of how lay and scholarly communities respond when informed of the contested scope of abrogation claims — does legal certainty remain valued once its contested textual basis is disclosed?',
    'If disclosure would not change adherence to the doctrine, the tradeoff is genuinely valued and the coordination function is robust; if disclosure would shift preference toward harmonizing readings, the doctrine''s persistence depends partly on non-disclosure of its contested basis, which would raise the effective suppression assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legal_certainty_versus_theological_coherence_tradeoff, preference, 'Whether the legal-certainty-for-flexibility tradeoff would survive full disclosure of the doctrine''s contested textual basis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__classical_abrogation, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(nask_tr_t0, projected).
narrative_ontology:measurement(nask_tr_t200, naskh_principle__classical_abrogation, theater_ratio, 200, 0.15).
narrative_ontology:measurement_basis(nask_tr_t200, projected).
narrative_ontology:measurement(nask_tr_t500, naskh_principle__classical_abrogation, theater_ratio, 500, 0.2).
narrative_ontology:measurement_basis(nask_tr_t500, projected).
narrative_ontology:measurement(nask_tr_t900, naskh_principle__classical_abrogation, theater_ratio, 900, 0.24).
narrative_ontology:measurement_basis(nask_tr_t900, projected).
narrative_ontology:measurement(nask_tr_t1200, naskh_principle__classical_abrogation, theater_ratio, 1200, 0.27).
narrative_ontology:measurement_basis(nask_tr_t1200, observed).
narrative_ontology:measurement(nask_tr_t1400, naskh_principle__classical_abrogation, theater_ratio, 1400, 0.28).
narrative_ontology:measurement_basis(nask_tr_t1400, observed).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__classical_abrogation, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(nask_be_t0, projected).
narrative_ontology:measurement(nask_be_t200, naskh_principle__classical_abrogation, base_extractiveness, 200, 0.38).
narrative_ontology:measurement_basis(nask_be_t200, projected).
narrative_ontology:measurement(nask_be_t500, naskh_principle__classical_abrogation, base_extractiveness, 500, 0.46).
narrative_ontology:measurement_basis(nask_be_t500, projected).
narrative_ontology:measurement(nask_be_t900, naskh_principle__classical_abrogation, base_extractiveness, 900, 0.5).
narrative_ontology:measurement_basis(nask_be_t900, projected).
narrative_ontology:measurement(nask_be_t1200, naskh_principle__classical_abrogation, base_extractiveness, 1200, 0.51).
narrative_ontology:measurement_basis(nask_be_t1200, observed).
narrative_ontology:measurement(nask_be_t1400, naskh_principle__classical_abrogation, base_extractiveness, 1400, 0.52).
narrative_ontology:measurement_basis(nask_be_t1400, observed).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__classical_abrogation, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(nask_su_t0, projected).
narrative_ontology:measurement(nask_su_t200, naskh_principle__classical_abrogation, suppression_requirement, 200, 0.42).
narrative_ontology:measurement_basis(nask_su_t200, projected).
narrative_ontology:measurement(nask_su_t500, naskh_principle__classical_abrogation, suppression_requirement, 500, 0.5).
narrative_ontology:measurement_basis(nask_su_t500, projected).
narrative_ontology:measurement(nask_su_t900, naskh_principle__classical_abrogation, suppression_requirement, 900, 0.55).
narrative_ontology:measurement_basis(nask_su_t900, projected).
narrative_ontology:measurement(nask_su_t1200, naskh_principle__classical_abrogation, suppression_requirement, 1200, 0.57).
narrative_ontology:measurement_basis(nask_su_t1200, observed).
narrative_ontology:measurement(nask_su_t1400, naskh_principle__classical_abrogation, suppression_requirement, 1400, 0.58).
narrative_ontology:measurement_basis(nask_su_t1400, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__classical_abrogation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(naskh_principle__classical_abrogation, 0.12).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__contextual_harmonization).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__progressive_restriction).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the naskh_principle kernel. classical_abrogation (this file) authors ε=0.52 for the classical supersession-hierarchy arrangement; contextual_harmonization and progressive_restriction author their own independent ε values for their respective standing arrangements (each reading's own operative doctrine, not classical_abrogation's endorsed alternative). All three should be network-linked to each other; see each file's commentary.kernel_context for the full sibling map.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
