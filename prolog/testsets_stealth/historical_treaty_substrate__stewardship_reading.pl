% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__stewardship_reading, []).

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
 *   constraint_id: historical_treaty_substrate__stewardship_reading
 *   human_readable: Historical Treaty Substrate — Shared Stewardship Reading
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional
 *
 * SUMMARY:
 *   Under the stewardship reading, the historical treaty substrate is a
 *   continuing relational pact: no cession of sovereignty occurred, the
 *   parties owe each other mutual obligations of coexistence, and territorial
 *   resources are to be governed jointly rather than unilaterally. The
 *   standing arrangement this story measures is how that substrate actually
 *   operates in contemporary settler states: Crown-administered tenure and
 *   unilateral resource allocation persist, consent is sought through
 *   consultative processes that do not transfer decision authority, and the
 *   pact's shared-governance content survives mainly in litigation,
 *   negotiation, and land defense. The extractiveness score is authored for
 *   this standing arrangement as the stewardship reading assesses it — the
 *   referent is the arrangement under contest, never the fully performed pact
 *   the reading endorses. Linked sibling files instantiate the other readings
 *   of the same kernel; their epsilon values differ because they instantiate
 *   different constraints, not different views of one. Time points index
 *   years since 1982 (t=0 is 1982, t=40 is 2022), spanning the era from
 *   constitutional entrenchment of treaty rights to the present
 *   reconciliation period.
 *
 * KEY AGENTS:
 *   - indigenous_nations_signatories: entitled party and cost-bearer (organized/identity_locked) — holds the pact's jurisdictional entitlements while absorbing the costs of its non-performance
 *   - settler_state_governments: administrator and residual claimant (institutional/arbitrage) — owes the pact's consent and shared-governance obligations while collecting territorial rents
 *   - settler_resource_industries: operational beneficiary (powerful/mobile) — extracts under tenures the obligated party issues without the required consent
 *   - settler_courts_judiciary: interpretive agenda-setter (institutional/constrained) — defines which pact obligations are enforceable and which are declared political
 *   - non_signatory_indigenous_nations: excluded party (organized/trapped) — subject to the substrate's operative assumptions without its protections
 *   - international_law_bodies: external reviewer (moderate/analytical) — monitors without enforcement power
 *   - comparative_treaty_scholars: analytical observer (analytical/analytical) — archives the gap between text and practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, 0.72).
domain_priors:suppression_score(historical_treaty_substrate__stewardship_reading, 0.74).
domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__stewardship_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__stewardship_reading, "Historical Treaty Substrate — Shared Stewardship Reading").
narrative_ontology:topic_domain(historical_treaty_substrate__stewardship_reading, "legal_anthropology/indigenous_law/comparative_constitutional").

domain_priors:requires_active_enforcement(historical_treaty_substrate__stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__stewardship_reading, '0199d660-c001-4cdc-b1fd-b3e26b8b2aaa').
narrative_ontology:cs_kernel_codification('0199d660-c001-4cdc-b1fd-b3e26b8b2aaa', fixed_text).
narrative_ontology:cs_authority_grounding('0199d660-c001-4cdc-b1fd-b3e26b8b2aaa', lineage).
narrative_ontology:cs_interpretation_layer_present('0199d660-c001-4cdc-b1fd-b3e26b8b2aaa').
narrative_ontology:cs_reading_relation('0199d660-c001-4cdc-b1fd-b3e26b8b2aaa', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('0199d660-c001-4cdc-b1fd-b3e26b8b2aaa', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('0199d660-c001-4cdc-b1fd-b3e26b8b2aaa', foundational, no_cession_of_inherent_sovereignty).
narrative_ontology:cs_axiom_status(no_cession_of_inherent_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('0199d660-c001-4cdc-b1fd-b3e26b8b2aaa', no_cession_of_inherent_sovereignty, deontological).
narrative_ontology:cs_axiom('0199d660-c001-4cdc-b1fd-b3e26b8b2aaa', foundational, consent_required_for_territorial_decisions).
narrative_ontology:cs_axiom_status(consent_required_for_territorial_decisions, holdable).
narrative_ontology:cs_axiom_grounding('0199d660-c001-4cdc-b1fd-b3e26b8b2aaa', consent_required_for_territorial_decisions, conventional).
narrative_ontology:cs_reference_frame('0199d660-c001-4cdc-b1fd-b3e26b8b2aaa', relational_pact_shared_stewardship).
narrative_ontology:cs_drift_state('0199d660-c001-4cdc-b1fd-b3e26b8b2aaa', contemporary_reconciliation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0199d660-c001-4cdc-b1fd-b3e26b8b2aaa', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, indigenous_nations_signatories).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_state_governments).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_resource_industries).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, indigenous_nations_signatories).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, non_signatory_indigenous_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, settler_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the pact's entitlements — continued territorial jurisdiction, reserved lands, and a required voice in resource decisions on their territories — transmitted through oral tradition and renewed in each generation's councils and ceremonies. They pursue honor-of-the-crown litigation, negotiate implementation agreements, and mount land defense when development proceeds without their agreement, while absorbing the costs of the pact's non-performance: dispossession proceeds faster than recognition. Leaving the relationship is not a live option; it constitutes their political identity and carries their inherent-rights claims, and every available channel — domestic courts, international bodies, direct action — operates inside structures the other party administers.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, indigenous_nations_signatories, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, indigenous_nations_signatories, payer).

% Administer the territory and its resource economy under asserted Crown title: grant tenures, collect royalties, police the land, and fund the courts. Under the pact they owe consent-seeking and shared-governance duties before territorial decisions; they meet these through consultation processes, settlement agreements, and periodic recognition instruments while retaining final decision authority. They can restructure their exposure through legislative amendment, judicial appointment, and doctrinal argument, and they control the interpretive forum in which the pact's meaning is settled.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state_governments, payer,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, settler_state_governments, beneficiary).

% Operate forestry, mining, energy, and agricultural enterprises under Crown-granted tenures issued without the consent the pact requires of the issuing party. Capital relocates between projects and jurisdictions when local opposition raises costs; the tenure system follows them.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_resource_industries, beneficiary,
    powerful, biographical, mobile, global).

% Interpret the treaty texts and craft the doctrines — honor of the crown, the duty to consult, justiciability limits, remedial discretion — that determine which of the pact's obligations are enforceable and which are declared political. Bound by precedent, constitutional structure, and the remedies available to courts; they cannot decline the interpretive role, and their declarations bind the administration only so far as the administration chooses to comply.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_courts_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Never signed the historical treaties but live under the same asserted Crown sovereignty and the same tenure system. They hold no treaty forum in which to press their own account of the relationship, litigate title from a weaker doctrinal starting position, and watch their territories allocated through instruments their nations never agreed to.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, non_signatory_indigenous_nations, excluded,
    organized, generational, trapped, regional).

% Review state conduct against instruments the state has adopted, issue findings and recommendations, and provide forums where Indigenous nations argue the relationship's terms without the domestic forum's constraints. They hold no enforcement power over the territory; their influence runs through reputation, reporting, and domestic incorporation.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, international_law_bodies, observer,
    moderate, generational, analytical, global).

% Document the substrate's operation across settler states, compare treaty text against practice, and archive the oral-history record alongside the written one. They neither collect nor pay; their accounts feed litigation, negotiation, and international review.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, comparative_treaty_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__stewardship_reading, settler_state_governments).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates durable coexistence between two peoples occupying one territory: defines respective jurisdictions, channels consent before territorial-resource decisions, provides dispute-resolution forums, and assigns mutual obligations — solving the collective-action problem of two sovereign orders sharing land without merging or annihilating either.
% TRANSFER_FUNCTION: Moves territorial-resource decision rights and consent obligations between the parties: under the pact, decision authority flows toward joint bodies and the administering state owes consent-seeking; as operated, effective control and resource value flow to settler institutions while formal entitlements rest with the signatory nations.
% ABSENT_VOICES: Non-signatory Indigenous nations are outside the pact's protections entirely; Indigenous women were historically sidelined from negotiation and from the annuity and payment structures; future generations of both peoples have no seat in the interpretive forums deciding how long the obligations run. They are absent from the courts, the negotiating tables, and the legislative record — present only as intervenors, plaintiffs, and protesters.
% DISAPPEARANCE_RATIONALE: If the substrate vanished overnight, the asserted Crown title underlying the entire property regime would lose its legal foundation, every resource tenure issued on treaty territories would be clouded, the constitutional settlements that absorbed the treaties would unravel, and the territorial order of the settler states would face reorganization from the ground up.
% FOUNDING_PROBLEM: Organizing durable coexistence between Indigenous nations and arriving settling powers on shared territory: securing peace, defining land relations between peoples who neither merged nor evacuated, and enabling trade and settlement without permanent frontier war.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: the treaty texts and commissioners' reports record mutual-assurance language on both sides; contemporaneous military and diplomatic correspondence acknowledges the nations as political communities; the Royal Proclamation text recognizes pre-existing possessory rights; and ongoing litigation dockets, royal commissions, and international treaty-body reviews all attest that the coexistence problem remains unresolved. No corroborating source attests that the founding problem was solved and closed.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__stewardship_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(historical_treaty_substrate__stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__stewardship_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because territorial value continues to flow to settler institutions while the pact's consent and shared-governance obligations go substantively unmet; the accumulation is slow because recognition instruments periodically claw back marginal ground. Suppression (0.74) is a raw structural property, unscaled by power or scope: it reflects the enforcement machinery — militarized responses to land defense, injunction regimes, criminalization of blockade, administrative control statutes — that holds the unilateral-allocation pattern in place against sustained objection. The suppression series is deliberately non-monotonic: a spike at t=8 (the 1990 armed standoff at Oka), partial relaxation through t=16 as the litigation channel opened after the landmark recognition decisions, then a ratchet from t=24 onward as injunction-based enforcement matured and culminated in the 2020 militarized policing of unceded territory. Theater rises steadily from 0.22 to 0.52: consultation processes, land acknowledgments, and reconciliation instruments increasingly substitute for transferred authority — the form of the pact is performed while its decision-sharing content stalls. Accessibility collapse is moderate (0.48): alternatives exist (courts, international bodies, political mobilization, economic leverage) but full exit from the substrate is impossible and the interpretive forum belongs to the other party. Resistance is high (0.70) because coalition capacity is real — cross-nation alliances, mass movements, and litigation coalitions have repeatedly forced doctrinal concessions. All three tracked series run on one shared six-point grid; every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the settler-government seat the substrate presents as a bundle of costly obligations wrapped around a genuine benefit: legitimacy, stable relations, and a resource economy it administers — a hybrid it experiences as manageable through consultation. From the signatory-nations seat the same structure presents as entitlement-plus-dispossession: rights acknowledged in doctrine while the land is allocated regardless. The judiciary experiences neither extraction nor subsidy but an interpretive mandate it cannot decline and cannot fully enforce. The excluded nations experience pure imposition: the substrate's operative assumptions govern their territories though they never entered its protections. The engine computes these per-seat classifications from the power, exit, and role data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations place settler_state_governments and settler_resource_industries near the beneficiary end: the state is the residual claimant on territorial value (tenures, royalties, fiscal control) and the industries hold arbitrage-grade mobility that pushes them nearest the full-beneficiary pole. The signatory nations appear on both sides — beneficiaries of the pact's entitlements, victims of its non-performance — so their derived directionality sits intermediate, pushed toward the target end by identity_locked exit: the relationship is constitutive, so exit is unthinkable even where it is formally possible. The non-signatory nations, listed only as victims with trapped exit, sit near the full-target end despite never having consented. The judiciary and the observer seats carry roughly symmetric or analytical directionalities: they administer and document the arrangement without collecting its rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — organizing coexistence between two peoples on shared territory — is live, so there is no mandatrophy to resolve: the arrangement has not outlived its function; its function is unfulfilled. The classification risk runs in both directions. Reading the substrate as a settled transaction (the sibling reading's implicit move) would grant it mountain-like immunity and erase the ongoing obligations; reading it as pure extraction-cover would erase the recoverable coordination function that makes renewal possible. The tangled_rope claim holds both: genuine mutual obligations capable of performance, and substantial current extraction sustained by enforcement. The drift risk is toward theatrical maintenance: theater_ratio has crossed 0.5, and if consultation is purely documentary (see the consultation_binding_force omega), the pact's administered form will persist while its function hollows — the piton path, with the added wrinkle that the administrator is also the principal beneficiary, which would make that endpoint a captured piton rather than a neglected one. The status-by-verdict pairing (live founding problem, world_rearranges) is internally consistent and raises no zombie flag; the theater trajectory is the early-warning signal to watch instead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_routing,
    'This constraint instantiates the stewardship_reading of the historical_treaty_substrate kernel. What changes structurally if a sibling reading is adopted instead — extinguishment_reading (treaties as completed property transactions: Indigenous parties leave the beneficiary set, the obligation set closes, and the resulting property regime claims settled-law status) or nation_to_nation_reading (treaties as international agreements between sovereign equals: consent rights retained but enforcement relocated to international fora)?',
    'Observe which reading the authoritative interpreter adopts: doctrinal movement in the settler courts, legislative implementation choices, and the framing of settlement instruments reveal which account of the kernel governs.',
    'Adopting the extinguishment reading collapses this constraint''s beneficiary set and recasts the standing arrangement''s extraction as settled law immune from obligation; adopting the nation-to-nation reading strengthens consent rights and changes the enforcement venue without adding the relational-stewardship content. The disagreement is located in a single structural element: whether the treaty texts effected a cession of sovereignty, which determines who sits in the beneficiary and obligation sets today.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_routing, conceptual, 'Committer-frame routing: one kernel, three readings, structurally distinct constraints.').

omega_variable(
    consent_scope_ambiguity,
    'Which territorial decisions does the pact''s consent requirement reach — all resource development affecting treaty territories, or only the categories the parties understood at negotiation?',
    'Triangulation of archival negotiation records, contemporaneous correspondence, and oral histories admitted under the evidentiary standards the recognition jurisprudence established.',
    'Sets the width of the settler state''s obligation set: a narrow scope shrinks the extraction attributable to non-consented development; a broad scope raises it and widens the gap the standing arrangement must close.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_scope_ambiguity, empirical, 'Breadth of the consent obligation the pact imposes on the administering state.').

omega_variable(
    oral_tradition_evidentiary_parity,
    'Do oral histories carry evidentiary parity with the written colonial record in determining the pact''s terms?',
    'Appellate treatment of oral-history evidence across jurisdictions, and whether procedural rules admit it at equal weight or as corroboration only.',
    'If parity is denied, the pact''s terms are recoverable only through the other party''s archive, and the asymmetry compounds — the reading''s normative content becomes unverifiable inside the forum that adjudicates it, inflating measured extraction uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oral_tradition_evidentiary_parity, empirical, 'Whether the pact''s terms can be established on evidence independent of the administering party''s records.').

omega_variable(
    consultation_binding_force,
    'Does the duty-to-consult apparatus ever constrain outcomes, or does it merely document decisions already made?',
    'Outcome-tracking studies comparing consultation-process objections with final approval decisions, and remedial rates when courts find the duty breached.',
    'Drives the theater_ratio trajectory: if consultation is purely documentary, the substrate is drifting toward theatrical maintenance of the pact''s form — the piton path — while extraction continues underneath.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consultation_binding_force, empirical, 'Whether the consultation layer performs the pact or substitutes for it.').

omega_variable(
    shared_jurisdiction_administrability,
    'Is joint territorial management administrable without decision gridlock, or does shared authority over resources impose coordination costs that no institutional design has yet solved?',
    'Comparative study of functioning co-management regimes: decision latency, dispute rates, and ecological and economic outcomes against unilaterally administered baselines.',
    'If joint management is administrable, the gap between pact and practice is attributable to evasion and the measured extraction stands; if it is not, part of that gap is genuine coordination cost rather than extraction — which changes how the standing arrangement''s epsilon should be interpreted without moving its referent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(shared_jurisdiction_administrability, preference, 'Whether the reading''s shared-governance content is workable, affecting attribution of the pact-practice gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__stewardship_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hts_stewardship_tr_t0, historical_treaty_substrate__stewardship_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(hts_stewardship_tr_t0, observed).
narrative_ontology:measurement(hts_stewardship_tr_t8, historical_treaty_substrate__stewardship_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(hts_stewardship_tr_t8, observed).
narrative_ontology:measurement(hts_stewardship_tr_t16, historical_treaty_substrate__stewardship_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(hts_stewardship_tr_t16, observed).
narrative_ontology:measurement(hts_stewardship_tr_t24, historical_treaty_substrate__stewardship_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement_basis(hts_stewardship_tr_t24, observed).
narrative_ontology:measurement(hts_stewardship_tr_t32, historical_treaty_substrate__stewardship_reading, theater_ratio, 32, 0.5).
narrative_ontology:measurement_basis(hts_stewardship_tr_t32, observed).
narrative_ontology:measurement(hts_stewardship_tr_t40, historical_treaty_substrate__stewardship_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement_basis(hts_stewardship_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(hts_stewardship_be_t0, historical_treaty_substrate__stewardship_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement_basis(hts_stewardship_be_t0, observed).
narrative_ontology:measurement(hts_stewardship_be_t8, historical_treaty_substrate__stewardship_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement_basis(hts_stewardship_be_t8, observed).
narrative_ontology:measurement(hts_stewardship_be_t16, historical_treaty_substrate__stewardship_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement_basis(hts_stewardship_be_t16, observed).
narrative_ontology:measurement(hts_stewardship_be_t24, historical_treaty_substrate__stewardship_reading, base_extractiveness, 24, 0.69).
narrative_ontology:measurement_basis(hts_stewardship_be_t24, observed).
narrative_ontology:measurement(hts_stewardship_be_t32, historical_treaty_substrate__stewardship_reading, base_extractiveness, 32, 0.71).
narrative_ontology:measurement_basis(hts_stewardship_be_t32, observed).
narrative_ontology:measurement(hts_stewardship_be_t40, historical_treaty_substrate__stewardship_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement_basis(hts_stewardship_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(hts_stewardship_su_t0, historical_treaty_substrate__stewardship_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(hts_stewardship_su_t0, observed).
narrative_ontology:measurement(hts_stewardship_su_t8, historical_treaty_substrate__stewardship_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement_basis(hts_stewardship_su_t8, observed).
narrative_ontology:measurement(hts_stewardship_su_t16, historical_treaty_substrate__stewardship_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement_basis(hts_stewardship_su_t16, observed).
narrative_ontology:measurement(hts_stewardship_su_t24, historical_treaty_substrate__stewardship_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement_basis(hts_stewardship_su_t24, observed).
narrative_ontology:measurement(hts_stewardship_su_t32, historical_treaty_substrate__stewardship_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(hts_stewardship_su_t32, observed).
narrative_ontology:measurement(hts_stewardship_su_t40, historical_treaty_substrate__stewardship_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement_basis(hts_stewardship_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__stewardship_reading, resource_allocation).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__nation_to_nation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'historical treaties' conflates three structurally distinct claims about one kernel: that the treaties completed a property transaction (extinguishment_reading), that they constituted international agreements between sovereign equals (nation_to_nation_reading), and that they established a continuing relational pact for shared stewardship (this file). Each reading instantiates a different constraint with its own epsilon, beneficiary/victim structure, and type — this reading authors high epsilon for the standing arrangement because the pact's consent and shared-governance obligations go unmet; the extinguishment reading authors near-zero epsilon for a completed transaction; the nation-to-nation reading authors intermediate epsilon with a different enforcement venue. The upstream member is the extinguishment reading, whose account has been the operationally dominant one and shapes the environment in which the other two are argued. All family members are linked via affects_constraints; no story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
