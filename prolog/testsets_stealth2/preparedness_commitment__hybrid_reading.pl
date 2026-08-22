% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__hybrid_reading, []).

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
 *   constraint_id: preparedness_commitment__hybrid_reading
 *   human_readable: Layered Disaster Preparedness Regime — Hybrid Reading (Memorial Commitment + Competent Function)
 *   domain: institutional/commitment-systems/disaster-preparedness
 *
 * SUMMARY:
 *   The standing arrangement under contest is the institutionalized layered
 *   preparedness regime: a founding catastrophe was converted into an annual
 *   memorial calendar (commemoration days, memorial exercises, museums,
 *   public campaigns) running alongside a standing competence layer (drills,
 *   stockpiles, warning systems, trained career responders, code
 *   enforcement). This file instantiates the hybrid_reading of the
 *   preparedness_commitment kernel: memorial elements stabilize
 *   cross-generational commitment, competence elements maintain operational
 *   function, and the tension between the layers — shared budgets, competing
 *   calendars, memorial visibility substituting for capability in funder
 *   oversight — is the arrangement's standing maintenance cost. Per the
 *   epsilon-invariance rule, the sibling readings (husk_reading,
 *   competence_reading) are separate constraint files linked in
 *   network.affects_constraints; this story authors one stable epsilon for
 *   the layered arrangement as THIS reading assesses it. The contest over the
 *   memorial layer's causal status is carried in the omega variables, not
 *   averaged into the metrics here.
 *
 * KEY AGENTS:
 *   - emergency_management_agency: agenda-setter/administrator (institutional / identity-locked) — runs both layers, owns the memorial calendar and the mandate structure
 *   - fiscal_authorities: co-agenda-setter and funder (institutional / arbitrage) — funds the arrangement, trims it in quiet decades, consumes memorial visibility as cheap verification in place of capability audits
 *   - competence_practitioners: primary bearer of the tension cost (organized / identity-locked) — career responders whose training time and equipment budget compete directly with memorial scheduling
 *   - memorial_apparatus: collects the memorial budget share (moderate / mobile) — museums, commemoration organizers, event vendors, public-affairs offices
 *   - general_public: protected and paying (organized / constrained) — receives the competence layer's protection, funds both layers through taxes and calendar obligations
 *   - local_governments: mandated implementer (organized / constrained) — files plans, runs local drills, hosts memorial events largely on own funds
 *   - underserved_communities: highest-risk residents with no seat in planning (powerless / trapped)
 *   - future_residents: inherit maintained or decayed capacity (powerless / trapped) — present only as the arrangement's stated purpose
 *   - preparedness_researchers: analytical observer (analytical / analytical) — measure drill realism against ceremony across jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, 0.48).
domain_priors:suppression_score(preparedness_commitment__hybrid_reading, 0.37).
domain_priors:theater_ratio(preparedness_commitment__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, suppression_requirement, 0.37).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(preparedness_commitment__hybrid_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_commitment__hybrid_reading, "Layered Disaster Preparedness Regime — Hybrid Reading (Memorial Commitment + Competent Function)").
narrative_ontology:topic_domain(preparedness_commitment__hybrid_reading, "institutional/commitment-systems/disaster-preparedness").

domain_priors:requires_active_enforcement(preparedness_commitment__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__hybrid_reading, '1132c64d-e4e9-47c8-8853-c00a98f6328c').
narrative_ontology:cs_kernel_codification('1132c64d-e4e9-47c8-8853-c00a98f6328c', formalized).
narrative_ontology:cs_authority_grounding('1132c64d-e4e9-47c8-8853-c00a98f6328c', lineage).
narrative_ontology:cs_interpretation_layer_present('1132c64d-e4e9-47c8-8853-c00a98f6328c').
narrative_ontology:cs_reading_relation('1132c64d-e4e9-47c8-8853-c00a98f6328c', preparedness_commitment__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('1132c64d-e4e9-47c8-8853-c00a98f6328c', preparedness_commitment__competence_reading, forecloses).
narrative_ontology:cs_axiom('1132c64d-e4e9-47c8-8853-c00a98f6328c', foundational, memorial_elements_stabilize_commitment).
narrative_ontology:cs_axiom_status(memorial_elements_stabilize_commitment, holdable).
narrative_ontology:cs_axiom_grounding('1132c64d-e4e9-47c8-8853-c00a98f6328c', memorial_elements_stabilize_commitment, empirically_contingent).
narrative_ontology:cs_axiom('1132c64d-e4e9-47c8-8853-c00a98f6328c', foundational, neither_layer_alone_suffices).
narrative_ontology:cs_axiom_status(neither_layer_alone_suffices, holdable).
narrative_ontology:cs_axiom_grounding('1132c64d-e4e9-47c8-8853-c00a98f6328c', neither_layer_alone_suffices, empirically_contingent).
narrative_ontology:cs_reference_frame('1132c64d-e4e9-47c8-8853-c00a98f6328c', layered_commitment_capability_regime).
narrative_ontology:cs_drift_state('1132c64d-e4e9-47c8-8853-c00a98f6328c', contemporary_post_founding_generation, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('1132c64d-e4e9-47c8-8853-c00a98f6328c', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__hybrid_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, general_public).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, emergency_management_agency).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, future_residents).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, memorial_apparatus).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, competence_practitioners).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, local_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_commitment__hybrid_reading, competence_practitioners).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, fiscal_authorities).
narrative_ontology:constraint_victim(preparedness_commitment__hybrid_reading, general_public).
narrative_ontology:constraint_vindicates(preparedness_commitment__hybrid_reading, commemoration_sustains_preparedness_commitment).
narrative_ontology:constraint_vindicates(preparedness_commitment__hybrid_reading, layered_redundancy_outperforms_single_layer_designs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the national preparedness regime end to end: designates the memorial calendar, mandates planning and drill cycles, maintains stockpiles and warning systems, and reports preparedness status upward. Its budget and statutory existence depend on the layered arrangement continuing; reorganizing around only one layer would dissolve the dual mandate the agency is built around. Exit would mean the agency ceasing to be what it is.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, emergency_management_agency, agenda_setter,
    institutional, generational, identity_locked, national).

% Set the budget envelope for preparedness each cycle under pressure from every other claim. In quiet decades they trim preparedness lines first because the costs of doing so are invisible and deferred; after events they restore them under public pressure. The memorial layer gives them a cheap, visible indicator that preparedness exists, which they consume as verification in place of costlier capability audits — and they pay for both layers from the same envelope.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, fiscal_authorities, agenda_setter,
    institutional, immediate, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, fiscal_authorities, payer).

% Lives under the protection the competence layer provides and pays for the whole arrangement through taxes, drill participation, and attention to annual commemorations. Cannot opt out of the taxes or the hazard exposure; can and does skip ceremonies. Benefits immediately from maintained capability and bears the memorial layer's cost as taxes and calendar obligations.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, general_public, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, general_public, payer).

% Career responders, drill instructors, engineers, and logistics staff who maintain the live capability. Their training hours compete directly with memorial scheduling inside a fixed budget, so they bear the tension cost concretely — fewer realistic exercises, older equipment, less scenario time. Their professional identity is constituted by the preparedness mission; leaving the corps would mean leaving the identity, so they absorb the tension cost rather than exit. They also draw their livelihood and purpose from the arrangement's continuation.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, competence_practitioners, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__hybrid_reading, competence_practitioners, beneficiary).

% Museums, commemoration organizers, public-affairs offices, and event vendors who produce the memorial layer: anniversary ceremonies, memorial exercises, exhibitions, campaigns. Receives a dedicated share of the preparedness budget for this work. The skills transfer to other event and communications work, so exit is mobile in principle, but the revenue is anchored to the memorial calendar.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, memorial_apparatus, beneficiary,
    moderate, biographical, mobile, national).

% Must file preparedness plans, run local drills, host memorial events, and maintain local stockpiles under national mandates, largely on their own funds or matched grants. They carry the compliance cost of both layers at the point of delivery and have no authority to reweight the layers; their leverage is limited to implementation pace and grant negotiation.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, local_governments, payer,
    organized, generational, constrained, regional).

% Live in the highest-risk districts — steep slopes, floodplains, aging housing, language-isolated neighborhoods — where real events do the most damage. Drill scenarios and memorial programming underweight their needs, and they hold no seat in the planning committees that allocate exercise time and stockpile placement. They would object that preparedness is distributed toward the visible and the ceremonial; they are outside the room where that allocation is made, and relocating away from the hazard is not within their means.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, underserved_communities, excluded,
    powerless, biographical, trapped, local).

% Will inherit either the maintained capability or its decayed remnant depending on how the layers hold over the coming decades. They are present in the arrangement only as its stated purpose — commemorations are addressed to them — and have no seat, no exit, and no voice except through the memorial layer's claim to act on their behalf.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, future_residents, beneficiary,
    powerless, generational, trapped, national).

% Comparative disaster scholars and auditors who measure drill realism against ceremony, track capability decay across quiet decades, and run the cross-jurisdiction comparisons that renewal debates cite. They collect no budget from the arrangement and bear none of its costs; their stake is the accuracy of the record.
narrative_ontology:constraint_stakeholder(preparedness_commitment__hybrid_reading, preparedness_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__hybrid_reading, memorial_apparatus).
narrative_ontology:fixing_cost_class(preparedness_commitment__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains response capacity for rare, high-impact events across political and generational turnover: the memorial layer keeps the commitment to prepare politically alive between disasters, and the competence layer keeps the capability real when commitment is tested. Neither problem can be solved once; both require continuous renewal.
% TRANSFER_FUNCTION: Moves budget, personnel time, and public attention from taxpayers and working practitioners into two channels: maintained capability (training, stockpiles, warning systems, code enforcement) and commemorative and drill performance (anniversary exercises, memorial events, plan filings). The memorial channel's budget is collected by the commemoration apparatus and consumed as commitment maintenance and as verifiable compliance evidence for funders.
% ABSENT_VOICES: The residents of future disaster paths — especially the underserved communities that real events hit hardest but whose needs are underweighted in drill scenarios — are not in planning rooms. Competence practitioners are present but structurally outvoted when memorial scheduling competes with training time; their objection surfaces mainly in post-event inquiries.
% DISAPPEARANCE_RATIONALE: Without the layered arrangement, quiet-decade budget raids would succeed within a few cycles: the memorial layer no longer makes preparedness visible, fiscal authorities redirect funds, drills lapse, stockpiles expire, and the next major event finds degraded capability — restarting the post-disaster inquiry cycle from a lower baseline. The arrangement's disappearance is exactly the abandonment its memorial layer exists to prevent.
% FOUNDING_PROBLEM: A founding catastrophe exposed total unpreparedness: no warning, no drills, no stockpiles, and no institutional memory of the previous event's lessons. The arrangement was built so that the disaster's memory would be institutionalized — commemorated annually — and converted into standing capability before the next one.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: post-event inquiry commissions repeatedly document that jurisdictions with maintained capability suffered materially lower losses and that quiet-decade decay preceded the worst outcomes; insurance loss data and comparative disaster studies — researchers with no budget stake in the arrangement — independently attest both the founding problem and its continuing recurrence. No serious party disputes that rare high-impact events recur; the live dispute is over what maintains readiness, not whether the problem exists.
narrative_ontology:disappearance_verdict(preparedness_commitment__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_commitment__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__hybrid_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_commitment__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48: the memorial budget share plus the inter-layer friction is a real, ongoing cost borne by identifiable seats, but under this reading both layers produce what they claim — the cost is the price of a functioning layered system, not rent. Suppression is 0.37: mandates (planning filings, drill participation, memorial observance, grant conditions) are real but mild, widely accepted, and leave design alternatives open. Theater_ratio 0.40 at interval end: a large minority of memorial and drill activity is performative — drills timed for cameras, plans filed to satisfy audits — while the majority still feeds real capability or real commitment. Accessibility_collapse 0.30: layer reweighting, community-based preparedness, and competence-only designs remain live alternatives. Resistance 0.45: quiet-decade budget-raiding pressure and drill fatigue are chronic, and the memorial layer exists substantially to overcome exactly this resistance — its commitment function and the resistance it faces are two descriptions of the same pressure. CYCLE: the series shows two full disaster-cycle oscillations (renewal event at t~24, major event at t~42): enforcement and realism surge after events, decay through quiet decades, and each renewal re-legitimizes the memorial layer ('this is why we drill'), so the oscillation is partly the arrangement's own maintenance mechanism — intermittent reinforcement at institutional scale, not noise. All scalars report the interval-end state (t=60, a late-quiet phase); the series run on one shared time grid with every tracked metric authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agency seat the arrangement is the commitment architecture it administers — the memorial calendar is how preparedness survives quiet decades. From the practitioner seat the same calendar is lost training time inside a fixed budget. From the fiscal seat the memorial layer is a cheap verification instrument that substitutes for capability audits. From the apparatus seat it is a funded calendar. The divergence between the administrator's coordination experience and the practitioners' cost experience is this story's central measurement, and the engine computes it from the structural data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: the agency, the public, future residents, and the apparatus are declared beneficiaries (low d); practitioners and local governments are declared victims (high d). The interesting seats are the mixed ones: practitioners are declared victims but draw livelihood and purpose from the arrangement, so their d sits below the full-target end even with identity-locked exit amplifying toward it; the public is declared beneficiary but pays the taxes and attention funding both layers, sitting near symmetric; fiscal authorities appear on neither declaration list — they fund the arrangement and consume its visibility, a near-symmetric seat with a beneficiary tilt from the verification benefit. No directionality overrides are authored: the derivation from declarations plus exit options lands each seat correctly, and the same-atom divergence (two institutional agenda-setters with different structural relationships) is finer-grained than the override mechanism's power-atom key can express, so the declarations must carry it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — cross-generational readiness decay between rare events — is live, and the arrangement has not outlived its mandate; the tension cost is the standing price of the mandate, not evidence of mandate death. The classification discipline runs both directions: a pure-competence reading would book the entire memorial budget as extraction and read the arrangement as rent collection; a husk reading would read the whole arrangement as performance and book it as inertial theater. The tangled_rope claim holds both facts this reading asserts — genuine coordination in both layers, asymmetric extraction in the memorial channel and the tension cost — and the temporal series is what would reopen the question: if theater_ratio settled permanently above 0.5 while extractiveness kept climbing through quiet decades, the mandate would be decaying into performance; the current series shows renewal events still resetting the drift, which is the hybrid reading's core empirical bet.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the hybrid_reading instantiation of the preparedness_commitment kernel; what would the sibling readings (husk_reading, competence_reading) change structurally, and where is the disagreement located?',
    'Read against the sibling files preparedness_commitment__husk_reading and preparedness_commitment__competence_reading: husk_reading would score the memorial layer as pure performance (theater_ratio high, coordination function denied) and competence_reading would score it as dispensable overhead (cost concentrated in the memorial budget, coordination function located only in exercised knowledge). The disagreement is located at exactly one structural element: whether the memorial layer causally stabilizes commitment or merely produces the feeling of retention.',
    'If the husk_reading diagnosis is right, this story''s coordination claim collapses and the arrangement reclassifies toward a theater-dominant type; if the competence_reading diagnosis is right, the memorial budget re-derives as pure extraction and epsilon rises materially. This file''s epsilon (0.48) is authored only under the hybrid reading''s lights and is not an average across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of kernel preparedness_commitment; siblings husk_reading and competence_reading; disagreement located at the memorial layer''s causal status.').

omega_variable(
    memorial_causal_status,
    'Does commemoration causally stabilize preparedness commitment, or does it merely correlate with commitment (jurisdictions that care about preparedness also stage commemorations)?',
    'Natural experiments: jurisdictions that discontinued or lost memorial elements (institutional discontinuity, funding collapse, administrative merger) compared with matched jurisdictions that retained them, tracking subsequent budget share, drill realism, and capability decay across quiet decades.',
    'If correlation-only, the hybrid reading collapses toward the husk diagnosis, the memorial budget re-derives as extraction, and epsilon rises materially; if causal, the memorial layer''s coordination function is confirmed and the current epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_causal_status, empirical, 'Whether the memorial layer''s commitment-stabilization is causal or correlational.').

omega_variable(
    memorial_share_dose_response,
    'Is the memorial layer''s current budget share above the level required to stabilize commitment (excess captured by the apparatus) or below it (abandonment risk)?',
    'Cross-jurisdiction dose-response analysis between memorial spend share and quiet-decade capability retention, with the renewal-event loss record as the outcome measure.',
    'Locates the size of the extraction component inside the memorial budget and determines whether the arrangement drifts toward a ratcheting memorial share (hardening extraction) or back toward a leaner coordination profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_share_dose_response, empirical, 'Whether the memorial budget share is commitment-load-bearing or in excess of it.').

omega_variable(
    memorial_signaling_framing,
    'Is the memorial layer better framed as commitment infrastructure (this reading) or as a funder-facing verification technology — cheap observability that lets fiscal authorities accept ceremony in place of capability audits?',
    'Examine whether memorial activity tracks budget-cycle timing and funder reporting requirements more closely than it tracks the hazard calendar or response needs; if memorial output is tuned to oversight audiences, the signaling framing fits better.',
    'Under the signaling framing the memorial budget re-derives as oversight-laundering cost, epsilon rises, and the classification drifts toward a capture-dominant type; both framings are coherent readings of the same calendar, and this story commits to the commitment-infrastructure framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memorial_signaling_framing, conceptual, 'Framing under-determination: commitment infrastructure versus funder-facing signaling for the memorial layer.').

omega_variable(
    practitioner_identity_lock,
    'Is the competence practitioners'' identity-fusion with the preparedness mission load-bearing for the arrangement — do they absorb the tension cost only because exit is unthinkable?',
    'Compare regimes with career-identity response corps against professionalized shift-work corps: if the tension cost surfaces as explicit labor conflict where identity fusion is absent, the lock is load-bearing.',
    'If the identity frame dissolves, the tension cost converts from silent absorption into bargaining conflict; the payer seat''s effective burden hardens and the arrangement''s quiet-decade stability weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(practitioner_identity_lock, empirical, 'Whether practitioner identity-lock is what lets the arrangement run the tension cost through them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__hybrid_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_commit_hybrid_tr_t0, preparedness_commitment__hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(prep_commit_hybrid_tr_t6, preparedness_commitment__hybrid_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(prep_commit_hybrid_tr_t12, preparedness_commitment__hybrid_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(prep_commit_hybrid_tr_t18, preparedness_commitment__hybrid_reading, theater_ratio, 18, 0.34).
narrative_ontology:measurement(prep_commit_hybrid_tr_t24, preparedness_commitment__hybrid_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(prep_commit_hybrid_tr_t30, preparedness_commitment__hybrid_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(prep_commit_hybrid_tr_t36, preparedness_commitment__hybrid_reading, theater_ratio, 36, 0.39).
narrative_ontology:measurement(prep_commit_hybrid_tr_t42, preparedness_commitment__hybrid_reading, theater_ratio, 42, 0.29).
narrative_ontology:measurement(prep_commit_hybrid_tr_t48, preparedness_commitment__hybrid_reading, theater_ratio, 48, 0.35).
narrative_ontology:measurement(prep_commit_hybrid_tr_t54, preparedness_commitment__hybrid_reading, theater_ratio, 54, 0.41).
narrative_ontology:measurement(prep_commit_hybrid_tr_t60, preparedness_commitment__hybrid_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(prep_commit_hybrid_be_t0, preparedness_commitment__hybrid_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(prep_commit_hybrid_be_t6, preparedness_commitment__hybrid_reading, base_extractiveness, 6, 0.34).
narrative_ontology:measurement(prep_commit_hybrid_be_t12, preparedness_commitment__hybrid_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(prep_commit_hybrid_be_t18, preparedness_commitment__hybrid_reading, base_extractiveness, 18, 0.45).
narrative_ontology:measurement(prep_commit_hybrid_be_t24, preparedness_commitment__hybrid_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(prep_commit_hybrid_be_t30, preparedness_commitment__hybrid_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(prep_commit_hybrid_be_t36, preparedness_commitment__hybrid_reading, base_extractiveness, 36, 0.49).
narrative_ontology:measurement(prep_commit_hybrid_be_t42, preparedness_commitment__hybrid_reading, base_extractiveness, 42, 0.41).
narrative_ontology:measurement(prep_commit_hybrid_be_t48, preparedness_commitment__hybrid_reading, base_extractiveness, 48, 0.45).
narrative_ontology:measurement(prep_commit_hybrid_be_t54, preparedness_commitment__hybrid_reading, base_extractiveness, 54, 0.5).
narrative_ontology:measurement(prep_commit_hybrid_be_t60, preparedness_commitment__hybrid_reading, base_extractiveness, 60, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(prep_commit_hybrid_su_t0, preparedness_commitment__hybrid_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(prep_commit_hybrid_su_t6, preparedness_commitment__hybrid_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(prep_commit_hybrid_su_t12, preparedness_commitment__hybrid_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(prep_commit_hybrid_su_t18, preparedness_commitment__hybrid_reading, suppression_requirement, 18, 0.42).
narrative_ontology:measurement(prep_commit_hybrid_su_t24, preparedness_commitment__hybrid_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(prep_commit_hybrid_su_t30, preparedness_commitment__hybrid_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(prep_commit_hybrid_su_t36, preparedness_commitment__hybrid_reading, suppression_requirement, 36, 0.38).
narrative_ontology:measurement(prep_commit_hybrid_su_t42, preparedness_commitment__hybrid_reading, suppression_requirement, 42, 0.58).
narrative_ontology:measurement(prep_commit_hybrid_su_t48, preparedness_commitment__hybrid_reading, suppression_requirement, 48, 0.48).
narrative_ontology:measurement(prep_commit_hybrid_su_t54, preparedness_commitment__hybrid_reading, suppression_requirement, 54, 0.4).
narrative_ontology:measurement(prep_commit_hybrid_su_t60, preparedness_commitment__hybrid_reading, suppression_requirement, 60, 0.37).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__hybrid_reading, seismic_building_standards).

% DUAL FORMULATION NOTE:
% The colloquial label 'the preparedness system' covers three structurally distinct claims about the memorial layer's function; per the epsilon-invariance principle they are authored as three files of one kernel family, linked here. The competence layer's empirical record (training and stockpiles reduce losses) is the shared upstream fact all three readings build on; the readings diverge on the memorial layer's causal status, which is where their epsilon values and victim structures diverge. The edge to seismic_building_standards records a structural influence: the competence layer's code-enforcement component depends on, and politically reinforces, the building-standards regime.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
