% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__retrospective_snare_exposure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__retrospective_snare_exposure, []).

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
 *   constraint_id: treaty_authority_cession__retrospective_snare_exposure
 *   human_readable: Treaty of Waitangi Authority Cession — Retrospective Snare Exposure Reading
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   The Treaty of Waitangi was signed in 1840 in two texts whose operative
 *   clauses diverge: the English text cedes 'all rights and powers of
 *   sovereignty,' while the Māori text grants 'kāwanatanga' (governorship)
 *   and guarantees 'tino rangatiratanga' over lands, villages, and taonga.
 *   This story instantiates the retrospective_snare_exposure reading of the
 *   treaty_authority_cession kernel: the textual divergence is not background
 *   noise but the extraction mechanism itself — chiefs assented to a
 *   governor, and the Crown operated on a cession of sovereignty they were
 *   never shown. Land purchasing under preemption, the Native Land Court's
 *   individualization of title, and successive legislative overrides
 *   extracted land and lawmaking authority under a consent that, on this
 *   reading, was never given to what was actually taken. The ε referent is
 *   the standing arrangement — Crown authority as operated on the English
 *   text — assessed by this reading's own lights; the consent-based
 *   constitutional settlement this reading would endorse is NOT the referent.
 *   Sibling readings (crown_cession_reading,
 *   rangatiratanga_retention_reading) are separate constraints, linked
 *   through the network. KEY AGENTS (by structural relationship): -
 *   crown_land_purchase_apparatus: agenda-setter and primary beneficiary
 *   (institutional/arbitrage) — administers the arrangement and collects the
 *   land - general_assembly_colonial_legislature: agenda-setter
 *   (institutional/arbitrage) — writes the override statutes when purchase
 *   stalls - european_settler_communities: beneficiary (organized/mobile) —
 *   receives the land, bears no enforcement burden -
 *   maori_rangatira_signatories: payer (organized/trapped) — signed a
 *   governorship, bore a sovereignty cession - maori_descendant_communities:
 *   payer (organized/identity_locked) — inherit the losses; exit would mean
 *   abandoning whakapapa - non_signatory_hapu: excluded and paying
 *   (organized/trapped) — bound without ever having signed -
 *   waitangi_tribunal: observer (institutional/analytical) — documents the
 *   divergence, recommends without binding
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.72).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.42).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.72).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty of Waitangi Authority Cession — Retrospective Snare Exposure Reading").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, '187f7fa5-b12c-4174-90f6-91ed07b051d8').
narrative_ontology:cs_kernel_codification('187f7fa5-b12c-4174-90f6-91ed07b051d8', fixed_text).
narrative_ontology:cs_authority_grounding('187f7fa5-b12c-4174-90f6-91ed07b051d8', extraction).
narrative_ontology:cs_interpretation_layer_present('187f7fa5-b12c-4174-90f6-91ed07b051d8').
narrative_ontology:cs_reading_relation('187f7fa5-b12c-4174-90f6-91ed07b051d8', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('187f7fa5-b12c-4174-90f6-91ed07b051d8', treaty_authority_cession__rangatiratanga_retention_reading, influences).
narrative_ontology:cs_axiom('187f7fa5-b12c-4174-90f6-91ed07b051d8', foundational, comprehension_constitutive_of_assent).
narrative_ontology:cs_axiom_status(comprehension_constitutive_of_assent, holdable).
narrative_ontology:cs_axiom_grounding('187f7fa5-b12c-4174-90f6-91ed07b051d8', comprehension_constitutive_of_assent, deontological).
narrative_ontology:cs_axiom('187f7fa5-b12c-4174-90f6-91ed07b051d8', foundational, textual_divergence_operates_as_extraction).
narrative_ontology:cs_axiom_status(textual_divergence_operates_as_extraction, holdable).
narrative_ontology:cs_axiom_grounding('187f7fa5-b12c-4174-90f6-91ed07b051d8', textual_divergence_operates_as_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('187f7fa5-b12c-4174-90f6-91ed07b051d8', informed_mutual_assent_cession).
narrative_ontology:cs_drift_state('187f7fa5-b12c-4174-90f6-91ed07b051d8', post_tribunal_historiography, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('187f7fa5-b12c-4174-90f6-91ed07b051d8', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchase_apparatus).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, european_settler_communities).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_rangatira_signatories).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_descendant_communities).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, non_signatory_hapu).
narrative_ontology:constraint_vindicates(treaty_authority_cession__retrospective_snare_exposure, english_text_supremacy_doctrine).
narrative_ontology:constraint_vindicates(treaty_authority_cession__retrospective_snare_exposure, crown_sovereignty_by_cession_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the treaty's operation: negotiated purchases under the preemption clause, ran the Native Land Court after 1865 to convert collective title into individualized alienable title, and fell back on legislation when ordinary purchase stalled. Collected land into the Crown estate and the funds that financed further settlement. Operates the English text wherever it confers authority and treats the Māori text's guarantees as non-justiciable wherever they would constrain.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchase_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchase_apparatus, beneficiary).

% Passed the statutes that operationalized the English text's sovereignty claim — the Native Land Acts, the validation of confiscations, the suppression measures — overriding customary authority whenever the purchase machinery's ordinary dealings failed. Its exit is trivial: any single statute can be amended or repealed without touching the underlying authority claim the statutes serve.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, general_assembly_colonial_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Received land surveyed out of Māori estates, elected the legislatures that wrote the override statutes, and supplied the demand-side pressure that kept purchase prices low. Their stake was upside-only: they bore no enforcement burden and could always leave the colony for Australia or Britain if returns disappointed.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, european_settler_communities, beneficiary,
    organized, generational, mobile, national).

% Signed the Māori text understanding kāwanatanga as a governor's limited authority over the newcomers, with tino rangatiratanga over lands and villages expressly retained. Refusal risked losing trade and the protection the Crown offered against speculators and rival powers, and there was no alternative counterparty offering a comparable compact. They subsequently bore the loss of land and lawmaking authority under a document that, in the language they could read, ceded nothing of the kind.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_rangatira_signatories, payer,
    organized, generational, trapped, regional).

% Inherited the outcome: fragmented and diminished landholdings, exclusion from the legal order that adjudicated the taking, and a constitutional position settled without their ancestors' informed consent. Whakapapa ties to land and to the treaty relationship are constitutive of who they are — leaving the arrangement would mean abandoning identity, not changing jurisdiction. Remedy channels exist inside the Crown's own institutions, on terms the Crown sets.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_descendant_communities, payer,
    organized, generational, identity_locked, national).

% Hapū whose rangatira never signed — some regions collected no signatures at all — yet who were brought under Crown authority and the land-purchase machinery regardless. Their standing objection, that no compact binds those who never entered it, has no seat in the operative framework; it survives in petitions, oral history, and the archival record of refusals.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, non_signatory_hapu, excluded,
    organized, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__retrospective_snare_exposure, non_signatory_hapu, payer).

% Investigates Crown conduct against the treaty's principles, receives claimant evidence including oral history, and has reported extensively on the textual divergence and its consequences. Recommends remedies; cannot compel the Crown to adopt them, and its mandate is constituted by the same parliamentary sovereignty whose foundation it examines.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, waitangi_tribunal, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchase_apparatus).
narrative_ontology:fixing_cost_class(treaty_authority_cession__retrospective_snare_exposure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a single governing framework for relations between the Crown and hapū — trade, law enforcement, and land transactions conducted under one authority instead of ad hoc dealings between settlers, speculators, and independent rangatira.
% TRANSFER_FUNCTION: Moved land, resources, and lawmaking authority from Māori collectives to the Crown and through it to the settler economy; moved the power to make binding law from rangatira to the colonial legislature.
% ABSENT_VOICES: Non-signatory hapū, whose refusal left no trace in the operative framework; rangatira who signed under time pressure or without comprehension of the English claim; and the generations whose inheritance was decided at signing. Their objections are recorded in petitions, oral testimony, and Tribunal submissions, outside the constitutional conversation that settled their position.
% DISAPPEARANCE_RATIONALE: If the cession-based authority vanished overnight, Crown title to most of the country's land would lose its legal foundation, Parliament's jurisdiction over Māori affairs would collapse, and the entire property-law and constitutional order would have to be rebuilt on negotiated consent rather than assumed succession from 1840.
% FOUNDING_PROBLEM: Securing a stable framework for organized settlement: preventing frontier violence between settlers and hapū, curbing chaotic private land speculation, and preempting rival imperial powers — with the official rationale adding protection of Māori from the worst of the speculators.
% FOUNDING_PROBLEM_CORROBORATION: Independent historiography corroborates obsolescence: Waitangi Tribunal findings and academic histories (the settlement-protection and rival-power rationales) date the resolution of the founding problems to within decades of signing, while documenting the arrangement's intensification afterward. Māori scholarship from outside the benefiting parties attests the protective purpose was never operative as claimed. No source outside the beneficiary set attests that the founding problems remain live.
narrative_ontology:disappearance_verdict(treaty_authority_cession__retrospective_snare_exposure, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__retrospective_snare_exposure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(treaty_authority_cession__retrospective_snare_exposure, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__retrospective_snare_exposure, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__retrospective_snare_exposure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__retrospective_snare_exposure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.72 at interval end, peaking 0.88 during the great alienation era) because the arrangement moved nearly the entire Māori land base and exclusive lawmaking authority under a consent that did not extend to what was taken. Suppression (0.42 currently, peaking 0.70 during the invasion-and-confiscation period) is authored as a raw structural property and is deliberately NOT scaled by power or scope — the engine owns that arithmetic. Theater ratio rises monotonically from 0.25 to 0.55: the ceremonial-consent layer that opened the arrangement (gifts, feasts, 'he iwi kotahi tatou' rhetoric) matures into apology-and-partnership performance layered over an unchanged authority claim — Goodhart drift in which the legitimating proxy progressively displaces the substance it originally stood in for. Accessibility collapse is moderate (0.60): understanding the divergence does not dissolve the constraint, because the available remedy channels run through institutions constituted by the contested authority itself. Resistance is high (0.75): the flagstaff wars, the Kingitanga, Parihaka, the 1975 land march, Bastion Point, the foreshore and seabed hīkoi — the constraint has been actively contested across the whole interval. All three temporal series share one ten-point grid (1840–2026) so no metric is ever sampled against another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute different types from identical facts. From the Crown apparatus's seat the arrangement is the lawful foundation it administers — continuity, title, jurisdiction — and the Māori text is a translation curiosity; from the signatories' and descendants' seats the same structure is extraction running on a text they could not have read as ceding anything. The settler seat experiences pure benefit with mobile exit; the descendant seat experiences inherited loss with identity-locked exit; the non-signatory seat experiences obligation without bargain. The engine computes this divergence from power, exit position, and directional placement; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   crown_land_purchase_apparatus and european_settler_communities sit near the beneficiary pole (d near 0.0): the arrangement subsidizes them with land and jurisdiction, and the apparatus additionally holds the administrative levers. maori_rangatira_signatories and maori_descendant_communities sit near the target pole (d near 1.0): they bear the transfer, and their exit positions (trapped, identity_locked) place them at the full-target end rather than the mobile end — a mobile target could arbitrage away, these cannot. non_signatory_hapu are targets who never even received the consideration side of the exchange. The waitangi_tribunal is an analytical seat; directionality is undefined for observation. Scope amplification applies: the arrangement operates nationally, so verifying what was actually assented to is harder, and effective extraction scales upward modestly from base ε.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problems — orderly settlement, protection from speculator chaos, preemption of rival powers — were substantially resolved within decades of signing; the arrangement persisted and intensified afterward. That is precisely the signature the R5 mismatch consumer reads (founding_problem_status dead crossed with disappearance_verdict world_rearranges yields the capture/zombie flag). Classifying this as snare rather than rope blocks the coordination cover story ('the treaty brought law and order') from absorbing the extraction: a rope has no victim set, and this arrangement's victim set is the point. Classifying it as snare rather than piton blocks the inertia misread: the arrangement is not maintained because nobody benefits enough to fix it — gains flow to a named seat, and fixing is prohibitive precisely because the beneficiary's entire title edifice rests on the constraint staying as it is. Mandatrophy resolution here is exposure, not decay: the mandate died, the mechanism stayed, and the mechanism was always doing different work than the mandate described.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates one reading (retrospective_snare_exposure) of the kernel treaty_authority_cession; what structural differences would the sibling readings (crown_cession_reading, rangatiratanga_retention_reading) produce?',
    'Generate and compare the sibling stories: the crown reading authors low epsilon over a valid-cession referent; the retention reading authors a breached-partnership referent with asymmetric enforcement. Cross-reading comparison locates the disagreement in the assent-comprehension premise.',
    'Under the crown reading the arrangement computes as legitimate foundation with negligible extraction; under the retention reading as breached partnership (tangled_rope-shaped). This story''s snare verdict is conditional on the no-informed-assent premise holding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings change the referent and the verdict.').

omega_variable(
    contemporaneous_comprehension,
    'Did any material subset of signatory rangatira comprehend the English text''s sovereignty claim at the time of signing?',
    'Archival reconstruction of the explanations actually given at each signing — Williams''s varying oral explanations, missionary intermediation, and recorded chiefly objections and questions about the governor''s reach.',
    'Uniform non-comprehension sustains the pure snare reading; documented pockets of comprehension would regionalize the classification toward tangled_rope for those particular transactions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contemporaneous_comprehension, empirical, 'Whether the extraction was uniformly covert at the time of operation.').

omega_variable(
    visibility_type_question,
    'Does retrospective visibility of the mechanism change the constraint''s type, or only observers'' knowledge of it?',
    'Distinguish the historical arrangement (classified as it operated) from the present-day remedial regime (Tribunal, settlement process) as a separate successor constraint, and test whether the successor carries a sunset structure.',
    'If the remedial regime is a distinct transitional constraint, the snare verdict attaches to the historical arrangement and the present regime is evaluated separately as scaffold-or-snare rather than inflating or diluting this story''s metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(visibility_type_question, conceptual, 'Whether exposure transforms the constraint itself or only the analysis of it.').

omega_variable(
    internalized_legitimacy_quiescence,
    'Is the reduced intensity of active resistance in recent decades consent, exhaustion, or successful absorption of dispute channels into Crown-controlled institutions?',
    'Compare resistance trajectories before and after the opening of Tribunal channels; examine the spread of constitutional proposals generated outside the settlement framework against participation rates within it.',
    'If quiescence reflects channel capture rather than consent, effective suppression is higher than the scalar suggests and the snare remains fully operative beneath the procedural accommodation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_legitimacy_quiescence, empirical, 'Structural versus absorbed suppression in the contemporary phase.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 1840, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1840, 0.25).
narrative_ontology:measurement(trea_tr_t1863, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1863, 0.22).
narrative_ontology:measurement(trea_tr_t1886, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1886, 0.2).
narrative_ontology:measurement(trea_tr_t1909, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1909, 0.24).
narrative_ontology:measurement(trea_tr_t1932, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1932, 0.28).
narrative_ontology:measurement(trea_tr_t1955, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1955, 0.35).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1975, 0.42).
narrative_ontology:measurement(trea_tr_t1995, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1995, 0.48).
narrative_ontology:measurement(trea_tr_t2010, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2010, 0.52).
narrative_ontology:measurement(trea_tr_t2026, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2026, 0.55).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1840, 0.42).
narrative_ontology:measurement(trea_be_t1863, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1863, 0.61).
narrative_ontology:measurement(trea_be_t1886, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1886, 0.79).
narrative_ontology:measurement(trea_be_t1909, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1909, 0.88).
narrative_ontology:measurement(trea_be_t1932, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1932, 0.87).
narrative_ontology:measurement(trea_be_t1955, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1955, 0.83).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1975, 0.78).
narrative_ontology:measurement(trea_be_t1995, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1995, 0.73).
narrative_ontology:measurement(trea_be_t2010, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2010, 0.71).
narrative_ontology:measurement(trea_be_t2026, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2026, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1840, 0.35).
narrative_ontology:measurement(trea_su_t1863, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1863, 0.7).
narrative_ontology:measurement(trea_su_t1886, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1886, 0.65).
narrative_ontology:measurement(trea_su_t1909, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1909, 0.58).
narrative_ontology:measurement(trea_su_t1932, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1932, 0.55).
narrative_ontology:measurement(trea_su_t1955, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1955, 0.5).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1975, 0.46).
narrative_ontology:measurement(trea_su_t1995, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1995, 0.44).
narrative_ontology:measurement(trea_su_t2010, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2010, 0.43).
narrative_ontology:measurement(trea_su_t2026, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2026, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__retrospective_snare_exposure, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession__crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession__rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, native_land_court_title_individualisation).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, foreshore_seabed_legislative_override).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'Treaty of Waitangi authority cession' decomposes into three structurally distinct readings of one kernel, each with its own ε, beneficiary/victim structure, and classification. This file instantiates retrospective_snare_exposure (ε ≈ 0.72; snare; covert-at-operation mechanism exposed retrospectively). The sibling crown_cession_reading authors a low-ε valid-cession constraint; the sibling rangatiratanga_retention_reading authors a breached-partnership constraint. The upstream/downstream gradient runs from the crown reading (highest empirical confidence in its own frame, cited as settling the matter) through the retention reading to this one (most contested, most extractive). This story links to both siblings and to two downstream mechanism constraints (title individualization, foreshore and seabed override) that the divergence made operable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
