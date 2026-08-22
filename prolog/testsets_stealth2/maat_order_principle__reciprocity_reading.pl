% ============================================================================
% CONSTRAINT STORY: maat_order_principle__reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__reciprocity_reading, []).

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
 *   constraint_id: maat_order_principle__reciprocity_reading
 *   human_readable: Ma'at Reciprocity Covenant: Royal Obligations Under Cosmic Balance
 *   domain: political/religious (ancient Egypt)
 *
 * SUMMARY:
 *   The Ma'at order principle, read as a reciprocity covenant: the king's
 *   rule is legitimate only while he delivers justice, stability, and proper
 *   resource distribution, and the governed's support — labor, grain,
 *   deference — is conditional on that delivery. The standing arrangement
 *   under contest (the ε referent, assessed by this reading's own lights) is
 *   the Egyptian redistributive state: a genuine basin-wide coordination
 *   machine (flood management, famine buffering, adjudication) that
 *   simultaneously moves a large, asymmetric surplus upward to the royal
 *   household, temples, and scribal establishment. This file instantiates ONE
 *   reading of the kernel maat_order_principle; the sibling readings
 *   (divine_mandate_reading: the ruler above the norm by definition;
 *   distributed_maintenance_reading: obligation diffused across all stations)
 *   are separate constraints with different victim sets and different ε,
 *   linked via network.affects_constraints. KEY AGENTS (by structural
 *   relationship): - royal_dynasty_and_household: agenda-setter and principal
 *   collector (institutional/identity_locked) — administers the covenant and
 *   is simultaneously its bound party - temple_priesthood: validating
 *   beneficiary (institutional/constrained) — collects endowments, certifies
 *   observance - scribal_tax_apparatus: collecting beneficiary
 *   (organized/constrained) — assesses, records, remits -
 *   peasant_farming_households: primary payer (powerless/constrained) —
 *   surrenders surplus and labor for delivered order - corvee_laborers:
 *   primary payer (powerless/constrained) — drafted seasonal labor, the
 *   accountability mechanism's sharpest test case - provincial_nomarchs:
 *   dual-positioned intermediaries (organized/constrained) — remit upward,
 *   retain locally, defect when delivery fails - foreign_war_captives:
 *   excluded seat (powerless/trapped) — bear the arrangement's costs wholly
 *   outside the covenant circle - egyptological_analysts: analytical observer
 *   (analytical/analytical) — reads the inscriptional record.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, 0.49).
domain_priors:suppression_score(maat_order_principle__reciprocity_reading, 0.41).
domain_priors:theater_ratio(maat_order_principle__reciprocity_reading, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, extractiveness, 0.49).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__reciprocity_reading, "Ma'at Reciprocity Covenant: Royal Obligations Under Cosmic Balance").
narrative_ontology:topic_domain(maat_order_principle__reciprocity_reading, "political/religious (ancient Egypt)").

domain_priors:requires_active_enforcement(maat_order_principle__reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__reciprocity_reading, '07c25c4c-22ff-494a-95df-85c58a2e6228').
narrative_ontology:cs_kernel_codification('07c25c4c-22ff-494a-95df-85c58a2e6228', distributed).
narrative_ontology:cs_authority_grounding('07c25c4c-22ff-494a-95df-85c58a2e6228', lineage).
narrative_ontology:cs_interpretation_layer_present('07c25c4c-22ff-494a-95df-85c58a2e6228').
narrative_ontology:cs_reading_relation('07c25c4c-22ff-494a-95df-85c58a2e6228', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('07c25c4c-22ff-494a-95df-85c58a2e6228', maat_order_principle__distributed_maintenance_reading, influences).
narrative_ontology:cs_axiom('07c25c4c-22ff-494a-95df-85c58a2e6228', foundational, royal_failure_possibility_licenses_withdrawal).
narrative_ontology:cs_axiom_status(royal_failure_possibility_licenses_withdrawal, holdable).
narrative_ontology:cs_axiom_grounding('07c25c4c-22ff-494a-95df-85c58a2e6228', royal_failure_possibility_licenses_withdrawal, conventional).
narrative_ontology:cs_axiom('07c25c4c-22ff-494a-95df-85c58a2e6228', foundational, provision_duties_constitute_legitimate_office).
narrative_ontology:cs_axiom_status(provision_duties_constitute_legitimate_office, holdable).
narrative_ontology:cs_axiom_grounding('07c25c4c-22ff-494a-95df-85c58a2e6228', provision_duties_constitute_legitimate_office, deontological).
narrative_ontology:cs_reference_frame('07c25c4c-22ff-494a-95df-85c58a2e6228', reciprocal_obligation_covenant).
narrative_ontology:cs_drift_state('07c25c4c-22ff-494a-95df-85c58a2e6228', post_first_intermediate_restoration, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('07c25c4c-22ff-494a-95df-85c58a2e6228', '').
narrative_ontology:cs_kernel_id(maat_order_principle__reciprocity_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, royal_dynasty_and_household).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, temple_priesthood).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, scribal_tax_apparatus).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, peasant_farming_households).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, corvee_laborers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, provincial_nomarchs).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, provincial_nomarchs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the covenant's terms, directs the collection apparatus, commissions the monuments, and performs the ordering rites; simultaneously the covenant's named obligor — justice, stability, and provision are duties of the office, not graces. The office constitutes the holder: stepping outside it means ceasing to be king, and failed delivery invites withdrawal of support, rival legitimacy claims, and the collapse sequences the record preserves. Collects the largest share of the surplus into royal granaries, building programs, and the dynastic household.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, royal_dynasty_and_household, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, royal_dynasty_and_household, beneficiary).

% Receives endowments, tithes, and festival provisioning from the collected surplus; certifies the king's observance through oracle and ritual; interprets the norm in disputes and trains the literate class. Its economic base depends on the arrangement's continuance, but it holds a lever: oracle verdicts against kings are attested, and in crises the priesthood's validation can be withheld or transferred to a rival claimant.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, temple_priesthood, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, temple_priesthood, agenda_setter).

% Assesses harvests, registers labor drafts, keeps the granary ledgers, and remits quotas upward; salaried and provisioned from the very collections it administers. Literacy binds it to state and temple employment; in collapse phases it serves whichever local power pays, which is how provincial extraction reorganizes so quickly after central failure.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, scribal_tax_apparatus, beneficiary,
    organized, biographical, constrained, national).

% Surrender a substantial share of the harvest to state and temple granaries and supply sons for labor drafts; receive in return flood management, famine buffering from stored grain, judicial settlement, and protection. Flight to the frontier or another polity is possible but severs hereditary land tenure and kin networks, so exit is priced high. When delivery fails they reduce payments where distance permits, and the complaint literature shows grievances were composed, preserved, and remembered across generations.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, peasant_farming_households, payer,
    powerless, generational, constrained, regional).

% Drafted seasonally for monument, quarry, canal, and expeditionary work; fed and housed from state stores while deployed; desertion punishable. The Deir el-Medina tomb-builders' community is the limit case on record: facing ration arrears, they staged an organized sit-down refusal at the mortuary works — and the administration negotiated and delivered partial payment rather than punishing, because the covenant's terms made collective withdrawal legible as legitimate protest rather than crime.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, corvee_laborers, payer,
    powerless, immediate, constrained, regional).

% Administer provinces under central appointment: remit tax quotas upward, furnish military and labor levies, retain local shares and perquisites. Their position is temporally unstable — under a delivering king they are the covenant's middle tier; when central delivery fails they stop remitting, tithe locally, maintain their own retinues, and drift toward autonomy. The First Intermediate Period is this seat's defection cascade, and the Middle Kingdom's recentralization is its forced re-absorption.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, provincial_nomarchs, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__reciprocity_reading, provincial_nomarchs, payer).

% Taken in Nubian and Levantine campaigns and assigned to corvee gangs, estate labor, and garrison service. They stand wholly outside the covenant circle: no reciprocal claim, no festival share, no judicial standing, no grievance channel. They bear the arrangement's labor costs at full weight while every protective term of the covenant applies to others; their exclusion marks the boundary of the reciprocity norm's coverage.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, foreign_war_captives, excluded,
    powerless, immediate, trapped, continental).

% Read the inscriptional and papyrological record — tomb autobiographies, the Palermo Stone annals, complaint literature, the Deir el-Medina strike papyri, granary accounts, donation stelae — to reconstruct who paid, who received, whether relief was administered as duty or favor, and whether the accountability mechanism ever reached the sovereign himself.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, egyptological_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__reciprocity_reading, royal_dynasty_and_household).
narrative_ontology:fixing_cost_class(maat_order_principle__reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of Nile-basin agriculture at civilizational scale: basin-wide flood management, granary storage that buffers multi-year low-Nile sequences, dispute adjudication across villages and nomes, and defense coordination — solved once, centrally, instead of per-village.
% TRANSFER_FUNCTION: Moves grain surplus, seasonal labor service, and craft goods from farming households upward through the scribal collection apparatus to royal granaries, temple endowments, and monument construction; moves protection, flood management, famine relief, and judicial settlement back down — with the upward flow substantially larger and more reliable than the downward flow in collapse phases.
% ABSENT_VOICES: Foreign war captives would object that they bear the corvee with no covenant coverage at all; they are present in the record only as gang rosters and estate inventories. Village women hold and transmit property but hold no formal seat in the obligation negotiation. Provincial communities at the delivery frontier would testify that famine relief arrived late or not at all while collection never failed.
% DISAPPEARANCE_RATIONALE: If the covenant vanished overnight, the redistributive state, the temple economy, and dynastic legitimacy all lose their operating logic simultaneously: collection loses its justification, the granary system loses its provisioning rationale, the priesthood loses its validating function, and the political form fragments into the local autarkies the collapse periods preview — the world this arrangement organizes rearranges around whatever regional powers fill the vacuum.
% FOUNDING_PROBLEM: Unification of Upper and Lower Egypt created a polity spanning a river-basin whose agriculture lives or dies by coordinated flood management and famine buffering — a coordination load no village coalition or nome alliance could carry. The arrangement was built to solve basin-scale resource security and dispute resolution under a single ordering authority.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the material record itself: nilometer installations and flood records, excavated granary complexes sized for multi-year buffers, the Palermo Stone's famine annals, and the First Intermediate Period archaeological signature showing what the basin looks like when the arrangement lapses — regional famine, local fortification, autarky. Modern Egyptological analysis (settlement archaeology, administrative papyrology) attests from an analytical seat with no stake in the covenant's continuation.
narrative_ontology:disappearance_verdict(maat_order_principle__reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__reciprocity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__reciprocity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(maat_order_principle__reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__reciprocity_reading, 0.49, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(maat_order_principle__reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.49 at interval end) because the reciprocity norm genuinely caps the take: the flows upward are large, but the covenant obliges offsetting delivery, and the historical record shows the cap operating — the Deir el-Medina workforce's organized refusal over ration arrears was met with negotiation and partial redress, not massacre, because the covenant made withdrawal legible as legitimate protest. Suppression (0.41) is correspondingly moderate: coercion exists (compulsory labor drafts, punishable desertion) but the arrangement does not depend on sealing exits, and resistance is partially licensed by the covenant's own terms. Theater (0.27) is low-moderate: within the framework's own lights the ordering rites ARE the maintenance mechanism, though a performative surplus appears in decay phases. Accessibility_collapse (0.42) is low for a constructed norm: alternatives persist (local autonomy, flight, foreign polities), and the Amarna episode proves radical revision of the ritual kernel was possible. Resistance (0.48) is real and recurring: First Intermediate Period fragmentation, nomarch defection cascades, labor stoppages, complaint literature preserving grievance. CYCLICAL DYNAMICS: the series run on one shared nine-point grid (unit ≈ 30 years, T=0 ≈ 2620 BCE Old Kingdom consolidation to T=48 ≈ 1180 BCE Ramesside equilibrium) and show two full cycles — Old Kingdom apex → First Intermediate Period collapse → Middle Kingdom restoration → Second Intermediate Period fragmentation → New Kingdom imperial phase → Ramesside stabilization. The cycle's primary drivers are exogenous (succession shocks, Nile variability, peripheral incursions), but the oscillation has a reinforcement property the commentary flags: each restoration RE-SELLS the covenant — the myth of cosmic guarantee lets a new dynasty restart collection at favorable terms after a visible failure, so the reset itself sustains extraction capacity. Extraction, theater, and suppression all spike together in collapse phases (T=16, T=32): extraction continues while delivery fails, ritual performance substitutes for substance, and coercion replaces legitimacy. Base_properties are authored at the end-state T=48, a mid-restoration stable phase — NOT at a collapse trough, which would overstate the steady-state character. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope in the engine's computation. No directionality_overrides are declared: the beneficiary/victim declarations plus exit atoms derive each seat's d correctly, including the Pharaoh's pulled-off-the-pole position (collector AND bound party). The leveled coercion_grid block is deliberately omitted: this story's subject is the covenant's accountability dynamic, not level-differentiated coercive pressure, and the gradient track reporting OPEN is the honest state.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the agenda-setter seat, the arrangement is the coordination machine the throne operates and is bound by — a covenant it cannot exit without ceasing to be what it is. From the payer seats, the same structure is a large asymmetric transfer softened by real, occasionally enforceable returns. The excluded captive seat computes the starkest divergence: identical corvee burdens, zero covenant coverage, no offsetting returns — pure extraction with no reciprocity term at all. The nomarch seat shows temporal role instability the static dial-set backgrounds: the same actor is a remitting payer under a delivering king and a defector-turned-local-extractor when delivery fails. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low-d seats: the royal household (principal collector, though pulled off the pure-beneficiary pole by its own bound-party obligations), the priesthood (endowments plus validating function), and the scribal apparatus (salaried from its own collections). Victim declarations map to high-d seats: farming households (surplus plus labor surrendered, constrained exit via land tenure and kin networks) and corvee laborers (bodied extraction, nearest the full-target end among covenant members). The foreign captive seat sits at maximal d — outside the covenant, nothing flows back. Spatial scope amplifies effective extraction modestly for the national-scale collection apparatus; the regional payer seats bear the scaled take locally.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline cuts both ways here. Reading the arrangement as pure coordination (rope) would erase the identifiable payers who bear a large asymmetric transfer — the corvee rolls and tax ledgers are not voluntary contributions. Reading it as pure extraction (snare) would erase the accountability mechanism that actually operated: withdrawals got redress, collapses followed failed delivery, and restoration required renewed delivery rather than mere reconquest. The tangled_rope claim holds both facts. On mandatrophy: the founding problem (basin-wide coordination no village coalition could solve) remains live across the entire interval — the arrangement has not outlived its function, there is no sunset clause, and the theater spikes are episodic symptoms of collapse phases rather than terminal inertia; the function demonstrably recovers at each restoration, so this is not a piton trajectory. The R5 mismatch consumer will find status=live paired with verdict=world_rearranges — coherent, no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maat_kernel_reading_contestation,
    'This constraint is ONE reading of kernel maat_order_principle (reading: reciprocity_reading). What would change structurally under the sibling readings — divine_mandate_reading (ruler embodies Ma''at and cannot violate it by definition) and distributed_maintenance_reading (maintenance diffused across all stations)?',
    'Comparative seat analysis across the three files: identify which seats gain or lose enforcement rights and whether any seat retains a claim mechanism against the sovereign under each reading.',
    'Under divine_mandate_reading the Pharaoh exits the bound-party position entirely — no enforceable obligations, extraction ceiling removed, payer seats lose their claim. Under distributed_maintenance_reading accountability diffuses until no seat can be singled out for withdrawal. This file''s moderate epsilon and tangled_rope structure exist ONLY under the reciprocity reading''s assignment of enforceable obligations to the ruler.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maat_kernel_reading_contestation, conceptual, 'Committer structure: which reading of the Ma''at kernel is instantiated, and what the siblings would change.').

omega_variable(
    covenant_coverage_boundary,
    'Does the reciprocity covenant''s moderate extraction reflect the arrangement as such, or only its operation INSIDE the covenant circle — given that foreign war captives bear corvee and estate labor with no reciprocal claim at all?',
    'Compare extraction incidence on covenant members versus out-group laborers using campaign-deportee records, estate rosters, and ration lists from Nubian and Levantine captive populations.',
    'If out-group extraction is included in the measure, effective epsilon rises substantially and the excluded seats classify as pure extraction targets — the arrangement''s overall character shifts snare-ward even while its internal face remains reciprocal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_coverage_boundary, empirical, 'Whether the measured moderation is an artifact of the covenant''s membership boundary.').

omega_variable(
    duty_vs_grace_framing,
    'When the crown provides famine relief, judicial settlement, and flood management, is this framed and administered as a duty OWED under the covenant, or as a discretionary GRACE that sustains dependence?',
    'Analyze the self-presentation of relief inscriptions and donation stelae against ledger reality: does the record show obligation-discharge language (debts discharged, dues rendered) or benefaction language (gifts bestowed, favor granted)?',
    'Grace-framing converts reciprocity into patronage: the extraction ceiling dissolves because returns become revocable favors rather than enforceable claims, and the classification drifts toward extraction without accountability. Duty-framing preserves the cap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duty_vs_grace_framing, conceptual, 'Whether the downward flows are covenant obligations or revocable patronage.').

omega_variable(
    withdrawal_reach_limit,
    'Did withdrawal of support ever actually discipline a SOVEREIGN, or only local officials — is the Deir el-Medina strike pattern (ration arrears answered by negotiation) the ceiling of the accountability mechanism''s reach?',
    'Survey the full corpus of recorded collective withdrawal, tax refusal, and labor stoppage across the interval, coding each by the rank of the actor disciplined and the outcome.',
    'If withdrawal reaches only bureaucrats and never the throne, the extraction ceiling is weaker than the reading claims — the top seat''s effective extraction is uncapped in practice and the arrangement drifts snare-ward at the sovereign seat while remaining rope-like below.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_reach_limit, empirical, 'Whether the accountability mechanism binds the sovereign or only his agents.').

omega_variable(
    cosmic_law_vs_enacted_norm,
    'Is Ma''at, as invoked in this arrangement, a discovered cosmic regularity that binds the king externally, or an enacted political norm dressed in cosmic language to make the arrangement appear inevitable?',
    'Track whether appeals to Ma''at constrain royal action in documented disputes (oracle verdicts against kings, court rulings against crown interests, elite biographies claiming to have corrected royal injustice) or merely decorate royal self-presentation.',
    'If decorative, the constraint is a constructed arrangement wearing cosmic garb — its persistence is political, not natural, and the accountability mechanism is contingent on elite buy-in. If constraining, part of the measured structure approaches a genuine standing order that no occupant of the throne chose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cosmic_law_vs_enacted_norm, conceptual, 'Naturalness ambiguity: cosmic law versus enacted political norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__reciprocity_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__reciprocity_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(maat_tr_t4, maat_order_principle__reciprocity_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(maat_tr_t8, maat_order_principle__reciprocity_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(maat_tr_t16, maat_order_principle__reciprocity_reading, theater_ratio, 16, 0.58).
narrative_ontology:measurement(maat_tr_t24, maat_order_principle__reciprocity_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement(maat_tr_t32, maat_order_principle__reciprocity_reading, theater_ratio, 32, 0.55).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__reciprocity_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(maat_tr_t44, maat_order_principle__reciprocity_reading, theater_ratio, 44, 0.29).
narrative_ontology:measurement(maat_tr_t48, maat_order_principle__reciprocity_reading, theater_ratio, 48, 0.27).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__reciprocity_reading, base_extractiveness, 0, 0.47).
narrative_ontology:measurement(maat_be_t4, maat_order_principle__reciprocity_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(maat_be_t8, maat_order_principle__reciprocity_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(maat_be_t16, maat_order_principle__reciprocity_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(maat_be_t24, maat_order_principle__reciprocity_reading, base_extractiveness, 24, 0.43).
narrative_ontology:measurement(maat_be_t32, maat_order_principle__reciprocity_reading, base_extractiveness, 32, 0.61).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__reciprocity_reading, base_extractiveness, 40, 0.51).
narrative_ontology:measurement(maat_be_t44, maat_order_principle__reciprocity_reading, base_extractiveness, 44, 0.47).
narrative_ontology:measurement(maat_be_t48, maat_order_principle__reciprocity_reading, base_extractiveness, 48, 0.49).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__reciprocity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(maat_su_t4, maat_order_principle__reciprocity_reading, suppression_requirement, 4, 0.37).
narrative_ontology:measurement(maat_su_t8, maat_order_principle__reciprocity_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(maat_su_t16, maat_order_principle__reciprocity_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(maat_su_t24, maat_order_principle__reciprocity_reading, suppression_requirement, 24, 0.33).
narrative_ontology:measurement(maat_su_t32, maat_order_principle__reciprocity_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(maat_su_t40, maat_order_principle__reciprocity_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(maat_su_t44, maat_order_principle__reciprocity_reading, suppression_requirement, 44, 0.42).
narrative_ontology:measurement(maat_su_t48, maat_order_principle__reciprocity_reading, suppression_requirement, 48, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__reciprocity_reading, resource_allocation).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__reciprocity_reading, distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Ma'at' decomposes into three structurally distinct readings of one kernel, per the ε-invariance principle. This file (reciprocity_reading) authors ε for the standing arrangement AS THE RECIPROCITY READING SEES IT: a capped, accountable extraction with real enforcement rights held by the governed. divine_mandate_reading authors ε for an arrangement with NO enforceable obligations on the ruler (uncapped extraction, no victim-with-claim); distributed_maintenance_reading authors ε for a diffused-accountability arrangement (no seat singled out, extraction attribution dissolved). Same kernel, different victim sets, different ε — three files, mutually linked. The upstream/downstream structure: the reciprocity reading is the accountability-bearing variant; the divine mandate reading is frequently cited BY royal self-presentation as cover against precisely the claims this reading licenses, so this file's network edge documents the contest the label conceals.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
