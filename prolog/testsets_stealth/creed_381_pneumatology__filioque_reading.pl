% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__filioque_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creed_381_pneumatology__filioque_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: creed_381_pneumatology__filioque_reading
 *   human_readable: Filioque Clause and Magisterial Clarification Authority (Latin Reading)
 *   domain: historical_theology/ecclesiastical_authority/commitment_systems
 *
 * SUMMARY:
 *   The pneumatological clause of the Creed of 381 — 'who proceeds from the
 *   Father' — is a contested kernel; this file instantiates ONE reading of
 *   it, the filioque_reading: the clause as completed by 'and the Son,'
 *   joined to the claim that the papal and conciliar magisterium possesses
 *   authority to clarify what the fathers left implicit. Historically the
 *   reading was realized as a unilateral insertion into the Latin creed
 *   (Toledo 589, spread through Frankish usage, adopted at Rome c. 1014),
 *   defended by anathema and exclusion, and imposed as a condition of union
 *   at Lyon II (1274) and Florence (1439). The sibling readings —
 *   monoprocession_reading and ecumenical_reunion_reading — are separate
 *   constraint stories with their own epsilon values, beneficiary structures,
 *   and classifications; they are linked through network.affects_constraints
 *   and are not averaged into this file. The claim/metric gap is deliberate:
 *   the constraint is CLAIMED as tangled_rope (genuine confessional
 *   coordination coexisting with asymmetric, enforced transfer of
 *   definitional authority), while the metrics independently describe
 *   substantially costly, actively enforced operation — the engine measures
 *   the divergence. KEY AGENTS (by structural relationship): - papal_see:
 *   Agenda-setter and primary beneficiary (institutional/arbitrage) —
 *   administers the completed creed and collects the jurisdictional expansion
 *   - eastern_patriarchates: Primary target (institutional/constrained) —
 *   bear the override of conciliar consent - latin_episcopate: Secondary
 *   beneficiary (institutional/constrained) - frankish_imperial_court:
 *   Strategic beneficiary (powerful/mobile) - eastern_conciliar_theologians:
 *   Targeted resisters (moderate/identity_locked) -
 *   ordinary_western_faithful: Diffuse beneficiaries (moderate/constrained) -
 *   ecumenical_conciliar_method: Excluded non-agent seat — the bypassed
 *   consent mechanism - modern_ecumenical_commissions: Analytical observers
 *   (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__filioque_reading, 0.78).
domain_priors:suppression_score(creed_381_pneumatology__filioque_reading, 0.76).
domain_priors:theater_ratio(creed_381_pneumatology__filioque_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__filioque_reading, tangled_rope).
narrative_ontology:human_readable(creed_381_pneumatology__filioque_reading, "Filioque Clause and Magisterial Clarification Authority (Latin Reading)").
narrative_ontology:topic_domain(creed_381_pneumatology__filioque_reading, "historical_theology/ecclesiastical_authority/commitment_systems").

domain_priors:requires_active_enforcement(creed_381_pneumatology__filioque_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__filioque_reading, 'bd38ef50-0943-4248-b52d-f953ca42b4a0').
narrative_ontology:cs_kernel_codification('bd38ef50-0943-4248-b52d-f953ca42b4a0', fixed_text).
narrative_ontology:cs_authority_grounding('bd38ef50-0943-4248-b52d-f953ca42b4a0', lineage).
narrative_ontology:cs_interpretation_layer_present('bd38ef50-0943-4248-b52d-f953ca42b4a0').
narrative_ontology:cs_reading_relation('bd38ef50-0943-4248-b52d-f953ca42b4a0', creed_381_pneumatology__monoprocession_reading, forecloses).
narrative_ontology:cs_reading_relation('bd38ef50-0943-4248-b52d-f953ca42b4a0', creed_381_pneumatology__ecumenical_reunion_reading, influences).
narrative_ontology:cs_axiom('bd38ef50-0943-4248-b52d-f953ca42b4a0', foundational, spirit_procession_includes_son).
narrative_ontology:cs_axiom_status(spirit_procession_includes_son, holdable).
narrative_ontology:cs_axiom_grounding('bd38ef50-0943-4248-b52d-f953ca42b4a0', spirit_procession_includes_son, theological).
narrative_ontology:cs_axiom('bd38ef50-0943-4248-b52d-f953ca42b4a0', foundational, magisterial_clarification_of_implicit_doctrine).
narrative_ontology:cs_axiom_status(magisterial_clarification_of_implicit_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('bd38ef50-0943-4248-b52d-f953ca42b4a0', magisterial_clarification_of_implicit_doctrine, theological).
narrative_ontology:cs_reference_frame('bd38ef50-0943-4248-b52d-f953ca42b4a0', progressive_clarification_of_apostolic_deposit).
narrative_ontology:cs_drift_state('bd38ef50-0943-4248-b52d-f953ca42b4a0', contemporary_ecumenical_dialogue_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('bd38ef50-0943-4248-b52d-f953ca42b4a0', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__filioque_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, papal_see).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, latin_episcopate).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, frankish_imperial_court).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, eastern_patriarchates).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, eastern_conciliar_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, ordinary_western_faithful).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inserted and defends the completed clause in the Latin creed, and teaches that the see of Peter carries authority to unfold what the conciliar fathers left implicit. Sets the terms under which communion with Rome is granted; every union negotiation since Lyon has taken the completed text as a fixed demand. Gains direct jurisdictional reach wherever the completed creed is confessed, and its position is defined by administering the settlement rather than receiving it.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, papal_see, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__filioque_reading, papal_see, beneficiary).

% Confesses and enforces the completed creed in dioceses from Toledo to Cologne. Receives a uniformly binding confession — one text for catechesis, liturgy, and ordination — and shares in the doctrinal certainty the Roman see guarantees. Bears the local cost of enforcing conformity but did not set the terms; its own conciliar voice narrowed as Roman clarification expanded.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, latin_episcopate, beneficiary,
    institutional, generational, constrained, continental).

% Championed the completed clause as a mark of distinction from Byzantium during the Carolingian period, sponsoring its spread through royal chapels and synods after Rome itself had declined to adopt it. Gained a confessional banner for imperial identity; its stake was strategic rather than confessional, and its emphasis could shift as politics shifted.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, frankish_imperial_court, beneficiary,
    powerful, biographical, mobile, continental).

% Hold the creed of 381 in its received form and regard the addition as an amendment made without the consent the conciliar method requires. Bear the loss of a shared confession and the conversion of a common text into a boundary marker drawn against them. Exit from communion with Rome was exercised in 1054 at the price of schism — available, but catastrophic; within the arrangement they hold no vote on the text they are asked to confess.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, eastern_patriarchates, payer,
    institutional, civilizational, constrained, continental).

% Teach and defend the inviolability of the 381 text; their professional and spiritual identity is constituted by fidelity to the consent of the fathers. Prominent resisters — Photius in the ninth century, Mark of Ephesus at Florence — paid with condemnation and exile. Leaving the position would dissolve the very identity the position protects; remaining under Roman terms means confessing an amended text they hold to be unamendable.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, eastern_conciliar_theologians, payer,
    moderate, generational, identity_locked, continental).

% Recite the completed creed weekly and inherit a unified confessional identity. Receive the coordination benefit of one faith and one text while bearing diffuse indirect costs, including the crusading era justified in part by the schism the clause came to symbolize. Their practical choice set is whatever their clergy teach.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, ordinary_western_faithful, beneficiary,
    moderate, biographical, constrained, continental).

% The practice of settling doctrine by councils of the whole church, whose consent requirement the Latin insertion bypassed. Listed because it is the seat whose absence defines the dispute: it speaks only through the councils that did or did not ratify the completed text, and it ratified none of them.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, ecumenical_conciliar_method, excluded,
    institutional, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(creed_381_pneumatology__filioque_reading, ecumenical_conciliar_method).

% Joint Orthodox-Catholic dialogues examining whether the clause divides what the fathers intended united. Take testimony from all seats, publish agreed statements, and have proposed that continued insistence on the completed text is disciplinary rather than doctrinally necessary. Hold an analytical position: they collect nothing and pay nothing under the arrangement.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, modern_ecumenical_commissions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(creed_381_pneumatology__filioque_reading, papal_see).
narrative_ontology:fixing_cost_class(creed_381_pneumatology__filioque_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains one binding confession across the Latin communion: a single creedal text coordinates catechesis, liturgy, and ordination examinations, marking the boundary of orthodoxy identically from Toledo to Cologne.
% TRANSFER_FUNCTION: Moves definitional authority over the creed — what it says and what it may be made to say — from the whole church acting in ecumenical council toward the Roman see and its magisterium; moves doctrinal compliance from the Eastern churches to the Roman norm.
% ABSENT_VOICES: The Eastern patriarchates and the conciliar method they embody were absent when the clause entered the Latin creed — no ecumenical consent was sought or given. Their objection enters only retrospectively, through protest and schism rather than deliberation.
% DISAPPEARANCE_RATIONALE: If the completed clause and its magisterial enforcement vanished overnight, Latin liturgies would revert to the 381 text, the papal claim to unilateral clarification would lose its paradigm case, and the East-West fault line would lose its sharpest symbol. The entire edifice of doctrinal development built on this precedent of magisterial completion would require re-founding.
% FOUNDING_PROBLEM: Securing the full divinity of the Holy Spirit against subordinationist readings after Nicaea and Constantinople, and — in the Carolingian West — articulating the Son's co-principle of procession against perceived Greek subordinationism; concurrently, consolidating a Frankish-Roman confessional identity distinct from Byzantium.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the Council of Constantinople (879-880) acta attest the settled authority of the 381 text from the Eastern seat; independent critical patristics scholarship attests both the anti-subordinationist motive and the clause's separate political career in Frankish hands; the Florence acta record both sides' stated motives. No corroborating source outside the benefiting parties attests that the founding problem required unilateral amendment specifically — that link is asserted only by the parties that gained from it.
narrative_ontology:disappearance_verdict(creed_381_pneumatology__filioque_reading, world_rearranges).
narrative_ontology:founding_problem_status(creed_381_pneumatology__filioque_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(creed_381_pneumatology__filioque_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(creed_381_pneumatology__filioque_reading, 'none', 1).
narrative_ontology:epsilon_provenance(creed_381_pneumatology__filioque_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creed_381_pneumatology__filioque_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(creed_381_pneumatology__filioque_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(creed_381_pneumatology__filioque_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the referent is the standing arrangement — the clause as inserted and enforced — and even assessed within this reading's own lights, the arrangement transfers definitional sovereignty from four ancient sees acting together to a single chair acting alone; that polity change is the cost, independent of the doctrine's truth value. Suppression (0.76) is a raw structural property, unscaled by power or scope: anathema (Humbert 1054), exclusion from communion, and unions accepted under documented geopolitical duress. Theater ratio (0.42) is moderate: the doctrinal function is real — the completed creed genuinely coordinates Western catechesis and liturgy — but after the East is lost, weekly recitation increasingly performs identity rather than resolving any live coordination problem. Accessibility collapse (0.60) is partial and seat-dependent: within the Latin communion, reverting to the 381 text becomes confessionally unthinkable, yet the East maintains the alternative continuously, so alternatives never fully collapse. Resistance (0.75) is among the highest recorded: the Photian controversy, the 879-880 council reaffirming the received text, a millennium of Orthodox objection, and Mark of Ephesus standing alone at Florence. The suppression is predominantly structural (roughly 70 percent: legal-canonical barriers, coerced ratification, exclusion machinery) with an internalized remainder (roughly 30 percent: creedal recitation fused into confessional identity, carried by agents who have never known another text). Coalition potential among the victims existed — the 879-880 council was precisely such a coalition — and failed through political fragmentation rather than absence of will, which is why the payer seats remain individually constrained despite collective institutional weight. All three temporal series run on one shared seven-point grid (589-1439) so every metric is authored at every examined time point; the trajectories show a monotonic enforcement ratchet, not a cycle.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the divergence is the data. From the papal seat the arrangement is an anchor it built and administers: unity achieved, truth secured, arbitration-grade control of the rule itself. From the eastern payer seats the same structure operates as enforced override: consent bypassed, exit priced at schism. The western episcopate sits between — net beneficiaries of the unified confession who simultaneously surrendered their own conciliar voice, a grievance that surfaces centuries later as Gallicanism. Same-level differentiation: the Frankish court and the Eastern patriarchates both hold powerful-to-institutional standing, yet the court's exit is mobile (its stake strategic, its emphasis droppable) while the patriarchates' exit is constrained (their stake constitutive, their exit catastrophic) — identical nominal power, opposite effective positions. Identity-lock binds the eastern theologians ideologically and professionally: their self-concept is constituted through fidelity to the unamended text, so exit is not costly but self-dissolving; if that identity frame broke, their seat would migrate toward mobile and the payer coalition's feasibility would change materially.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. The papal_see sits nearest the beneficiary end (d near 0.0): it authors the rule, collects the jurisdictional expansion, and holds arbitrage-grade exit — it can redefine the terms of the dispute itself. The latin_episcopate derives low d (net gain, modest enforcement burden). The frankish_imperial_court derives low d with mobile exit damping further. The eastern_patriarchates derive d near 1.0: they bear the transfer of definitional authority, and constrained exit (schism as the only door) amplifies their effective position toward the full-target end. The eastern_conciliar_theologians sit at the full-target end with identity_lock amplifying further — trapped agents read nearer full target than mobile ones. The ordinary_western_faithful sit near symmetric: genuine coordination benefit received, diffuse indirect costs borne. Universal papal scope claims amplify effective extraction on the targets, since verifying consent at universal scope is impossible and the verification failure always runs against the seat that never voted. No directionality overrides were needed: the structural declarations plus exit options produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing the Spirit's full divinity against subordinationism — has receded as an existential threat, but the parties dispute whether the doctrinal question it served is closed; hence founding_problem_status is contested rather than dead, and the status-times-verdict pair (contested x world_rearranges) correctly avoids the zombie flag while refusing a clean mandatrophy resolution. The tangled_rope classification is what prevents mislabeling in both directions: a pure-snare reading would erase the real coordination achieved (one confession genuinely binding a continent's teaching office), and a pure-rope reading would erase the overridden consent that the arrangement required enforcement to hold. The rising theater ratio alongside rising extractiveness traces the classic accumulation pattern — coordination layered with rent — without yet collapsing into piton, because the enforcement machinery remains load-bearing and no decay of function has occurred; the constraint is degraded relative to its founding warrant, not yet inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'Does the filioque_reading correctly instantiate the creed_381_pneumatology kernel, or does one of the sibling readings (monoprocession_reading, ecumenical_reunion_reading) better capture what the 381 fathers transmitted?',
    'Comparative analysis of the conciliar acta, pre-381 patristic procession language, and reception history across both communions, assessing which reading''s beneficiary/victim structure matches the text''s transmission.',
    'If the monoprocession reading prevails, the structure inverts: the East becomes the fixed-text defender and Rome the innovating party bearing the amendment burden. If the reunion reading prevails, the extraction dissolves entirely, since bilateral recognition replaces unilateral imposition and no seat pays what another collects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Which reading of the pneumatological kernel this constraint instantiates, and what inversion follows if a sibling prevails.').

omega_variable(
    doctrine_truth_vs_procedure_extraction,
    'Is the measured cost borne by the Eastern churches intrinsic to the double-procession doctrine itself, or to the unilateral procedure by which the clause was inserted and enforced?',
    'Counterfactual comparison: had the clause been adopted by a genuinely ecumenical council with documented Eastern consent, would the structural transfer of definitional authority still have occurred?',
    'If the procedure is the operative cost, doctrinal agreement alone cannot settle the dispute and the constraint persists as long as the pathway of decision persists; if the doctrine is, then the truth-value of double procession settles it and the procedural grievance is derivative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_truth_vs_procedure_extraction, conceptual, 'Whether the arrangement''s cost flows from propositional content or from decision procedure.').

omega_variable(
    clarify_vs_amend_boundary,
    'Does authority to clarify implicit doctrine licitly extend to altering a conciliar text''s explicit wording, or are clarification and amendment categorically distinct acts?',
    'Hermeneutical reconstruction of what the 381 fathers held implicit, combined with magisterial-theoretical analysis of whether unfolding and rewriting share a single warrant.',
    'If amendment exceeds clarification, the reading''s second foundational axiom collapses and the arrangement reduces to imposition resting on enforcement alone; if clarification covers amendment, the arrangement''s cost is bounded by doctrinal necessity and partially excused.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(clarify_vs_amend_boundary, conceptual, 'Boundary between interpreting a fixed text and changing it, on which the reading''s authority claim turns.').

omega_variable(
    duress_reception_validity,
    'Do the unions accepted under political duress — Lyon II under Michael VIII''s survival needs, Florence under the Ottoman threat — constitute genuine reception of the completed creed?',
    'Post-hoc repudiation records: the immediate Eastern synodal rejections following both councils, culminating in the Constantinople synod of 1484 formally repudiating Florence.',
    'If duress voids reception, the arrangement never achieved voluntary uptake and its persistence rests wholly on coercive maintenance — the suppression metric understates the structural picture and the enforcement trajectory reads as pure ratchet rather than normalization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(duress_reception_validity, empirical, 'Whether assent extracted under geopolitical threat counts as the consent the arrangement claims.').

omega_variable(
    doctrinal_development_naturality,
    'Is the clause''s emergence organic development — the Vincentian unfolding of what was always believed — or constructed innovation retroactively legitimized?',
    'Trace citation patterns of procession language before and after 381 across Greek and Latin fathers; measure whether the Latin formula was latent in the received tradition or novel at insertion.',
    'An organic finding would approach the arrangement to a discovered rather than enacted structure from the Latin seat, lowering its constructedness; a novelty finding confirms the naturality claim as retrospective cover and strengthens the false-naturality reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_development_naturality, conceptual, 'Whether the clause presents itself as natural doctrinal fact while being an enacted institutional artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__filioque_reading, 589, 1439).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cree_tr_t589, creed_381_pneumatology__filioque_reading, theater_ratio, 589, 0.1).
narrative_ontology:measurement(cree_tr_t800, creed_381_pneumatology__filioque_reading, theater_ratio, 800, 0.15).
narrative_ontology:measurement(cree_tr_t869, creed_381_pneumatology__filioque_reading, theater_ratio, 869, 0.2).
narrative_ontology:measurement(cree_tr_t1054, creed_381_pneumatology__filioque_reading, theater_ratio, 1054, 0.28).
narrative_ontology:measurement(cree_tr_t1215, creed_381_pneumatology__filioque_reading, theater_ratio, 1215, 0.32).
narrative_ontology:measurement(cree_tr_t1274, creed_381_pneumatology__filioque_reading, theater_ratio, 1274, 0.38).
narrative_ontology:measurement(cree_tr_t1439, creed_381_pneumatology__filioque_reading, theater_ratio, 1439, 0.42).

% Extraction over time
narrative_ontology:measurement(cree_be_t589, creed_381_pneumatology__filioque_reading, base_extractiveness, 589, 0.3).
narrative_ontology:measurement(cree_be_t800, creed_381_pneumatology__filioque_reading, base_extractiveness, 800, 0.42).
narrative_ontology:measurement(cree_be_t869, creed_381_pneumatology__filioque_reading, base_extractiveness, 869, 0.5).
narrative_ontology:measurement(cree_be_t1054, creed_381_pneumatology__filioque_reading, base_extractiveness, 1054, 0.62).
narrative_ontology:measurement(cree_be_t1215, creed_381_pneumatology__filioque_reading, base_extractiveness, 1215, 0.7).
narrative_ontology:measurement(cree_be_t1274, creed_381_pneumatology__filioque_reading, base_extractiveness, 1274, 0.74).
narrative_ontology:measurement(cree_be_t1439, creed_381_pneumatology__filioque_reading, base_extractiveness, 1439, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(cree_su_t589, creed_381_pneumatology__filioque_reading, suppression_requirement, 589, 0.25).
narrative_ontology:measurement(cree_su_t800, creed_381_pneumatology__filioque_reading, suppression_requirement, 800, 0.35).
narrative_ontology:measurement(cree_su_t869, creed_381_pneumatology__filioque_reading, suppression_requirement, 869, 0.45).
narrative_ontology:measurement(cree_su_t1054, creed_381_pneumatology__filioque_reading, suppression_requirement, 1054, 0.6).
narrative_ontology:measurement(cree_su_t1215, creed_381_pneumatology__filioque_reading, suppression_requirement, 1215, 0.68).
narrative_ontology:measurement(cree_su_t1274, creed_381_pneumatology__filioque_reading, suppression_requirement, 1274, 0.72).
narrative_ontology:measurement(cree_su_t1439, creed_381_pneumatology__filioque_reading, suppression_requirement, 1439, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__filioque_reading, identity_coordination).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, monoprocession_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, ecumenical_reunion_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the creed_381_pneumatology kernel. The colloquial label 'what the creed says about the Spirit's procession' conflates three structurally distinct claims: the filioque_reading (this file — clause completed, magisterial clarification authority, papal see as beneficiary, Eastern sees as payers, high epsilon), the monoprocession_reading (clause as received; the fixed-text defenders become the beneficiaries of textual stability and the Latin innovator bears the amendment burden — the beneficiary/victim structure inverts), and the ecumenical_reunion_reading (bilateral recognition; extraction approaches zero because no seat pays what another collects). The upstream member is the text's original conciliar authority, cited by all three readings as evidence; the downstream members diverge on whether the text is completable, inviolable, or plural. Each story carries its own stable epsilon; the family is linked through affects_constraints so contamination propagates across readings rather than being averaged inside any one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
