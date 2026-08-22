% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__extinguishment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: historical_treaty_substrate__extinguishment_reading
 *   human_readable: Historical Treaties as Completed Cession Transactions (Extinguishment Reading)
 *   domain: legal/constitutional/historical
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the historical_treaty_substrate
 *   kernel: the extinguishment reading, under which the historical treaties
 *   are completed property transactions — the signatory nations ceded
 *   territorial sovereignty in exchange for defined reserves, annuities, and
 *   enumerated rights, and the settler state holds sole legitimate authority
 *   over the ceded territory. Per the epsilon-referent rule for kernel
 *   readings, extractiveness is authored for the standing arrangement under
 *   contest (the operative extinguishment settlement) as THIS reading
 *   assesses it: a completed, compensated exchange whose conceded wrongs are
 *   breaches of the consideration, not the transaction itself. The sibling
 *   readings are separate constraints with their own epsilon,
 *   beneficiary/victim structures, and classifications; they are linked
 *   through the network, not folded in. Under this reading's structural delta
 *   the Indigenous nations sit in the beneficiary set for the narrow treaty
 *   rights and outside the victim set entirely — the victims array is
 *   authored empty as this reading's explicit structural claim, and the omega
 *   variables carry what a sibling reading would change. Suppression is
 *   authored as a raw structural property and is not scaled by power or
 *   scope; only extractiveness is scaled, by the engine, from directionality
 *   and scope.
 *
 * KEY AGENTS:
 *   - crown_settler_state: primary beneficiary and agenda-setter (institutional/arbitrage) — receives ceded jurisdiction, administers the settlement, pays the defined consideration
 *   - indigenous_treaty_nations: beneficiary of the narrow treaty rights (organized/trapped) — hold reserves, annuities, and enumerated rights inside a jurisdiction they do not share and cannot exit
 *   - settler_courts_treaty_interpreters: agenda-setter over meaning (institutional/constrained) — adjudicate what the completed transactions mean; their authority rides on the settlement's finality
 *   - settler_landholders_resource_industries: derivative beneficiary (powerful/mobile) — hold fee-simple and resource title made certain by the cession
 *   - provincial_municipal_governments: beneficiary (institutional/mobile) — tax and govern the ceded territory under exclusive jurisdiction
 *   - indigenous_rights_advocates: excluded seat (organized/constrained) — press alternative readings; the settlement's finality defines them out of the conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, 0.2).
domain_priors:suppression_score(historical_treaty_substrate__extinguishment_reading, 0.35).
domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, rope).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Historical Treaties as Completed Cession Transactions (Extinguishment Reading)").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal/constitutional/historical").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, '048409c3-4df3-4e0c-8e07-d0abbf3c78ad').
narrative_ontology:cs_kernel_codification('048409c3-4df3-4e0c-8e07-d0abbf3c78ad', fixed_text).
narrative_ontology:cs_authority_grounding('048409c3-4df3-4e0c-8e07-d0abbf3c78ad', extraction).
narrative_ontology:cs_interpretation_layer_present('048409c3-4df3-4e0c-8e07-d0abbf3c78ad').
narrative_ontology:cs_reading_relation('048409c3-4df3-4e0c-8e07-d0abbf3c78ad', historical_treaty_substrate__nation_to_nation_reading, forecloses).
narrative_ontology:cs_reading_relation('048409c3-4df3-4e0c-8e07-d0abbf3c78ad', historical_treaty_substrate__stewardship_reading, forecloses).
narrative_ontology:cs_axiom('048409c3-4df3-4e0c-8e07-d0abbf3c78ad', foundational, treaty_cession_complete_and_compensated).
narrative_ontology:cs_axiom_status(treaty_cession_complete_and_compensated, holdable).
narrative_ontology:cs_axiom_grounding('048409c3-4df3-4e0c-8e07-d0abbf3c78ad', treaty_cession_complete_and_compensated, conventional).
narrative_ontology:cs_axiom('048409c3-4df3-4e0c-8e07-d0abbf3c78ad', secondary, sovereignty_claims_nonjusticiable_post_cession).
narrative_ontology:cs_axiom_status(sovereignty_claims_nonjusticiable_post_cession, holdable).
narrative_ontology:cs_axiom_grounding('048409c3-4df3-4e0c-8e07-d0abbf3c78ad', sovereignty_claims_nonjusticiable_post_cession, conventional).
narrative_ontology:cs_reference_frame('048409c3-4df3-4e0c-8e07-d0abbf3c78ad', completed_cession_settlement).
narrative_ontology:cs_drift_state('048409c3-4df3-4e0c-8e07-d0abbf3c78ad', post_s35_reconciliation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('048409c3-4df3-4e0c-8e07-d0abbf3c78ad', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, crown_settler_state).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_landholders_resource_industries).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, provincial_municipal_governments).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, indigenous_treaty_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, crown_settler_state).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, extinguishment_doctrine).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, settler_state_sole_jurisdiction).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, completed_transaction_finality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Executes and administers the completed transactions: surveys and holds reserve boundaries, pays the annuities, exercises exclusive legislative and judicial authority over the ceded territory, and interprets the treaties' meaning through its courts. Receives the ceded territorial jurisdiction and the title certainty that underwrites the entire property regime; pays the defined consideration as the honored terms of the completed exchange. Can restructure the arrangement unilaterally through legislation and modern treaty-making.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, crown_settler_state, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, crown_settler_state, payer).

% Hold the consideration side of the completed transactions as this reading records it: defined reserve lands, annual payments, and enumerated harvesting and other rights, delivered and binding. Live entirely within the jurisdiction the transactions vested in the settler state; the settlement's terms are fixed and cannot be renegotiated as of right, and there is no jurisdiction outside the settlement to exit into. Successive generations inherit the arrangement and its constitutive relationship to the land without having been party to its making.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_treaty_nations, beneficiary,
    organized, civilizational, trapped, regional).

% Adjudicate what the completed transactions mean: which rights survive, whether obligations were breached, and whether any sovereignty question remains open. Their interpretive authority rests on the settlement's finality; each ruling either maintains or erodes the completed-transaction frame. They collect no rents from the arrangement, but cannot exit the interpretive role and are bound by the texts and prior rulings.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_courts_treaty_interpreters, agenda_setter,
    institutional, generational, constrained, national).

% Hold fee-simple and resource rights across the ceded territory, derived from the title certainty the completed transactions provide. Farm, log, mine, and build on uncontestable title; can transfer or exit any parcel at market value, and none of their holdings depend on ongoing negotiation with the nations.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_landholders_resource_industries, beneficiary,
    powerful, biographical, mobile, national).

% Tax, regulate, and service the ceded territory under the exclusive jurisdiction the transactions vested. Derive rate base, planning certainty, and administrative clarity from the settlement, and operate daily on the assumption that no competing jurisdiction exists within their boundaries.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, provincial_municipal_governments, beneficiary,
    institutional, generational, mobile, regional).

% Press for the treaties to be read as something other than completed cessions — as ongoing relationships or agreements between surviving polities. The settlement's finality defines them out of the conversation: within the extinguishment frame there is nothing to negotiate, so their claims reach the courts only as narrow rights-implementation questions. Organized across many nations; their mobilization is the visible resistance to the settlement's closure.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_rights_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__extinguishment_reading, crown_settler_state).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__extinguishment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converted open-ended, violence-prone territorial contestation between expanding settler polities and Indigenous nations into defined, administrable entitlements: fixed reserve boundaries, scheduled payments, enumerated rights, and exclusive allocation of jurisdiction — a solved-once allocation of territory and governing authority.
% TRANSFER_FUNCTION: Moves territorial sovereignty, jurisdiction, and the land base from the signatory nations to the settler state; moves defined reserves, annuity payments, and enumerated treaty rights from the settler state to the nations as the recorded consideration.
% ABSENT_VOICES: The nations-as-sovereign-polities: this reading's own premise defines them out of the conversation post-cession, so no seat within the frame speaks for continuing sovereignty. Also absent: the generations bound by the transactions without having been party to them, and the dissenting factions within the signatory nations who contested the treaties at signing and whose objections the written record largely omits.
% DISAPPEARANCE_RATIONALE: If the extinguishment settlement were undone overnight — if the completed transactions ceased to ground title and jurisdiction — the property regime of the entire ceded land base would lose its foundation: fee-simple title chains, resource tenures, municipal boundaries, and the state's legislative authority would all stand on reopened sovereignty claims, and every institution named in this story would have to renegotiate its position from zero.
% FOUNDING_PROBLEM: Resolving competing sovereignty and land claims between expanding settler polities and Indigenous nations without perpetual war — achieving a definitive transfer of territory and jurisdictional clarity through negotiated instruments.
% FOUNDING_PROBLEM_CORROBORATION: The treaty commissioners' reports and the written texts attest the cession framing, but they are the benefiting party's own record. Indigenous oral histories of the same negotiations — now admissible as evidence — attest a different founding problem: sharing of the land under an ongoing relationship, not a completed sale. No source outside the beneficiary set corroborates the completed-cession framing as the mutual founding understanding; that corroborative absence is itself the signal, and it is stated plainly rather than papered over.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__extinguishment_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__extinguishment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__extinguishment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(historical_treaty_substrate__extinguishment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__extinguishment_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__extinguishment_reading_tests).
:- end_tests(historical_treaty_substrate__extinguishment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics are authored from this reading's seat for the standing arrangement. Extractiveness 0.2: the reading holds the transaction complete and the consideration delivered; what it concedes as residue — shortchanged reserve surveys, lapsed agricultural and annuity terms, and the compounding gap between fixed nineteenth-century consideration and ceded-territory value — is real but bounded in its frame. Suppression 0.35: the reading justifies finality as the agreed term, yet concedes the arrangement has been actively enforced against those who would reopen it; the scalar records that coercive maintenance without adjudicating its justification. Theater 0.25: annuity payments, reserve administration, and rights adjudication are real functions; the ceremonial share (treaty-day performance, honour-of-the-Crown rhetoric outrunning conduct) is a minority. All three series run on one shared grid — time points 0, 25, 50, 75, 100, 125, 150 of a roughly 1871-to-2020s span in 25-year steps — so every metric is authored at every examined point. The suppression series is deliberately U-shaped: enforcement machinery built to a peak (movement passes, assembly and land-claims fundraising prohibitions), decaying through the mid-century liberalization era, then partially re-intensifying as enforcement of finality against reopening attempts (injunctions, justiciability limits) rather than a return of the historical administrative apparatus. The claimed type is rope — this seat holds the arrangement to be a completed, mutually entered exchange — and is stated independently of the metrics; where the engine's per-seat computation diverges from the claim, that divergence is the measurement the story exists to take.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently even within this reading's own data. From the crown_settler_state and settler_courts seats the settlement is a performed contract: obligations defined, consideration flowing, meaning adjudicated — the state's secondary payer role is the honored price of a bargain it struck. From the indigenous_treaty_nations seat the same structure reads differently: the beneficiary declaration damps their derived extraction, but trapped exit, fixed consideration, and inherited terms mean successive generations hold narrow entitlements inside a jurisdiction they never renegotiate — the engine should compute their seat as materially more burdened than the beneficiary label alone suggests. The settler_courts seat is the subtle divergence: the courts collect no rents, but their interpretive authority is constituted by the settlement's finality, giving them a structural stake in the kernel's stability that no beneficiary/victim declaration captures. The excluded advocates seat computes from outside the frame entirely. Inter-institutionally, the same-side institutions (state, courts, provinces) experience the constraint through different benefit channels — territory, interpretive authority, rate base — with correspondingly different stakes in its stability.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: crown_settler_state, settler_landholders_resource_industries, provincial_municipal_governments, and indigenous_treaty_nations are all declared beneficiaries, so all derive damped directionality — that is this reading's structural claim, made deliberately rather than by omission. The victims array is authored empty: the reading holds the cession was compensated and that no party bears uncompensated cost in the standing arrangement. Exit modulation does the residual work the declarations miss — the nations' trapped exit pushes their derived d toward the target end relative to the fully mobile settler beneficiaries, and the state's arbitrage-grade restructuring capacity holds it near the beneficiary end. No directionality overrides are authored: the per-power-atom override mechanism cannot separate the settler_courts seat (institutional, no declared material benefit) from the state and provinces (institutional, declared beneficiaries), so the courts' interpretive-authority stake is left to the structural derivation and flagged here and in the perspectival-gap analysis instead.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading declares the founding problem dead — the territorial contest the treaties were built to solve was, in its frame, resolved by completed transactions — while the disappearance verdict is world_rearranges. That status-dead plus verdict-rearranges mismatch is the honest authoring, not an error to be tuned away: the reading's contribution is to prevent mislabeling a genuine completed exchange as ongoing theft (the inverse of the usual mandatrophy failure), and its blind spot is mislabeling foreclosure as completion — a settlement can be simultaneously a real solved problem at founding and a maintained closure now. The sibling readings carry the correction: nation_to_nation reopens the founding problem as live (consent is ongoing), stewardship reopens it as contested (the transaction never occurred as described). The corpus should read this story's mismatch against its theater and suppression series rather than treating either the rope claim or the genealogy as self-certifying; the founding problem's corroborative absence from outside the beneficiary set is recorded in the six-questions interview and is itself signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading (extinguishment_reading) of the historical_treaty_substrate kernel; what structural changes follow if a sibling reading (nation_to_nation_reading or stewardship_reading) becomes operative instead?',
    'Adoption events: constitutional entrenchment of an alternative reading, judicial reinterpretation of the treaty texts, or treaty-renewal legislation; then observe whether Indigenous nations re-enter the victim set for territorial jurisdiction and the settler state loses sole-authority status.',
    'Under a sibling reading the beneficiary/victim structure inverts: the nations re-enter the victim set for jurisdiction, the settler state''s gain becomes contested extraction, epsilon for the same referent rises sharply, and the classification moves from rope toward tangled_rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: which kernel reading is instantiated and what a sibling reading would structurally change.').

omega_variable(
    consent_validity_at_signing,
    'Was the Indigenous parties'' consent to the cession terms validly obtained — did both parties understand and agree to the same transaction?',
    'Archival negotiation records, contemporaneous interpreter and missionary accounts, and admissible Indigenous oral histories of the negotiations; compare written cession clauses against recorded oral undertakings and the negotiating parties'' expressed understandings.',
    'If consent was substantially impaired, the completed-transaction premise fails on its own terms; epsilon rises sharply even within a transactional frame and the arrangement''s legitimacy requires renegotiation rather than continued performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_validity_at_signing, empirical, 'Whether the founding consent underlying the extinguishment reading was validly obtained.').

omega_variable(
    oral_written_understanding_divergence,
    'Do the written cession clauses or the oral undertakings preserved in Indigenous oral histories govern what was actually transacted?',
    'Oral-history evidence now admissible in treaty interpretation; systematic comparison of the written texts against recorded oral promises across the treaty series.',
    'If oral undertakings govern, the extinguishment reading''s textual foundation collapses and the standing arrangement has been performing the wrong contract for its entire interval; the divergence is extensively documented, so the open question is which record governs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oral_written_understanding_divergence, empirical, 'Written-text versus oral-understanding divergence as the basis of the transaction''s terms.').

omega_variable(
    fixed_consideration_adequacy,
    'Can a completed-transaction frame coherently tolerate retrospective assessment of whether fixed nineteenth-century consideration remains adequate to the ceded territory''s compounded value, or is adequacy foreclosed by finality itself?',
    'Comparative contract doctrine on unconscionability and changed circumstances applied to historic transactions; legislative or judicial adoption of a reopening standard for historic settlements.',
    'If adequacy is assessable, the standing arrangement accrues unpriced extraction as ceded-territory value compounds past the fixed consideration; if finality forecloses the question, the reading holds but the foreclosure itself becomes the contested extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fixed_consideration_adequacy, conceptual, 'Whether the completed-exchange frame can absorb a retrospective adequacy challenge to the consideration.').

omega_variable(
    finality_vs_suppression_persistence,
    'Does the settlement persist because both parties continue to benefit from it, or because alternative readings are foreclosed from operative law?',
    'Observe the arrangement''s stability as exit and voice open: if the operative core survives expanded oral-history admissibility, UNDRIP-style implementation, and modern-treaty precedent without coercive response, persistence is benefit-based; if each opening is met with enforcement (injunctions, justiciability limits), persistence is suppression-based.',
    'If suppression-based, the rope claim fails and the arrangement computes as tangled_rope or snare despite this reading''s beneficiary structure; if benefit-based, the rope claim is vindicated on this reading''s own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finality_vs_suppression_persistence, empirical, 'Whether persistence reflects mutual benefit or foreclosure of alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__extinguishment_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(hist_tr_t0, observed).
narrative_ontology:measurement(hist_tr_t25, historical_treaty_substrate__extinguishment_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(hist_tr_t25, observed).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__extinguishment_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement_basis(hist_tr_t50, observed).
narrative_ontology:measurement(hist_tr_t75, historical_treaty_substrate__extinguishment_reading, theater_ratio, 75, 0.35).
narrative_ontology:measurement_basis(hist_tr_t75, observed).
narrative_ontology:measurement(hist_tr_t100, historical_treaty_substrate__extinguishment_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement_basis(hist_tr_t100, observed).
narrative_ontology:measurement(hist_tr_t125, historical_treaty_substrate__extinguishment_reading, theater_ratio, 125, 0.27).
narrative_ontology:measurement_basis(hist_tr_t125, observed).
narrative_ontology:measurement(hist_tr_t150, historical_treaty_substrate__extinguishment_reading, theater_ratio, 150, 0.25).
narrative_ontology:measurement_basis(hist_tr_t150, observed).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(hist_be_t0, observed).
narrative_ontology:measurement(hist_be_t25, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 25, 0.2).
narrative_ontology:measurement_basis(hist_be_t25, observed).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement_basis(hist_be_t50, observed).
narrative_ontology:measurement(hist_be_t75, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 75, 0.3).
narrative_ontology:measurement_basis(hist_be_t75, observed).
narrative_ontology:measurement(hist_be_t100, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 100, 0.26).
narrative_ontology:measurement_basis(hist_be_t100, observed).
narrative_ontology:measurement(hist_be_t125, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 125, 0.22).
narrative_ontology:measurement_basis(hist_be_t125, observed).
narrative_ontology:measurement(hist_be_t150, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 150, 0.2).
narrative_ontology:measurement_basis(hist_be_t150, observed).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(hist_su_t0, observed).
narrative_ontology:measurement(hist_su_t25, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement_basis(hist_su_t25, observed).
narrative_ontology:measurement(hist_su_t50, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 50, 0.65).
narrative_ontology:measurement_basis(hist_su_t50, observed).
narrative_ontology:measurement(hist_su_t75, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 75, 0.5).
narrative_ontology:measurement_basis(hist_su_t75, observed).
narrative_ontology:measurement(hist_su_t100, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 100, 0.32).
narrative_ontology:measurement_basis(hist_su_t100, observed).
narrative_ontology:measurement(hist_su_t125, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 125, 0.3).
narrative_ontology:measurement_basis(hist_su_t125, observed).
narrative_ontology:measurement(hist_su_t150, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 150, 0.35).
narrative_ontology:measurement_basis(hist_su_t150, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__extinguishment_reading, resource_allocation).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate__nation_to_nation_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate__stewardship_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'the historical treaties' covers three structurally distinct constraints — three readings of one kernel (historical_treaty_substrate) — decomposed per the epsilon-invariance principle. This file instantiates the extinguishment_reading only: treaties as completed property transactions, with epsilon authored low from that seat's own lights and an explicitly empty victim set. The sibling stories (nation_to_nation_reading, stewardship_reading) share the referent — the same standing treaty arrangement — and author their own higher epsilon, their own beneficiary/victim structures (in which the nations re-enter the victim set for territorial jurisdiction), and their own classifications. The readings disagree on one located structural element: whether sovereignty was transferred and closed. They are linked here as a constraint family; no reading's data is folded into another's.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
