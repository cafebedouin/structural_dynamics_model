% ============================================================================
% CONSTRAINT STORY: constitutional_conventions__ministerial_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_conventions__ministerial_responsibility, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: constitutional_conventions__ministerial_responsibility
 *   human_readable: Ministerial Responsibility Convention
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   Ministerial responsibility is the constitutional convention that
 *   ministers answer to Parliament for their departments and resign for
 *   serious failure. It is one reading of the contested kernel of
 *   constitutional accountability structures in parliamentary systems. The
 *   convention establishes that the bureaucracy is not directly answerable to
 *   Parliament; instead, it is answerable through a minister who is both a
 *   politician and the formal head of a department. This creates a structural
 *   ambiguity: the minister may be a figure-head bearing responsibility for
 *   decisions made by permanent officials, or a genuine leader whose
 *   authority flows downward. The convention benefits the executive (by
 *   providing a visible accountability mechanism that can be invoked or
 *   suppressed as political advantage dictates) and the civil service (by
 *   remaining anonymous). It extracts cost from individual ministers who may
 *   resign for failures they did not author and cannot prevent. The
 *   convention also benefits Parliament (by providing a person to scrutinize
 *   and hold to account) but at the cost of accepting ministerial fig-leafing
 *   of bureaucratic failures. The theater ratio (0.65) reflects that formal
 *   doctrine (ministers resign for serious failure) is invoked unevenly —
 *   resignations are rarer than the doctrine suggests, and when they occur,
 *   they often track political events rather than failure severity.
 *
 * KEY AGENTS:
 *   - Individual Ministers: Primary victims (powerful/constrained) — bear public responsibility and resignation risk for departmental failures often authored by permanent officials
 *   - Civil Service Hierarchy: Primary beneficiary (powerful/constrained) — remain anonymous, shielded from direct parliamentary accountability, while maintaining influence over policy
 *   - Parliamentary Commons: Secondary beneficiary (organized/mobile) — gain a mechanism to hold executive accountable, though the mechanism often obscures rather than clarifies bureaucratic failure
 *   - Prime Minister/Executive: Primary beneficiary (institutional/arbitrage) — control both ministerial resignations (to satisfy Parliament) and civil service loyalty (by maintaining the fiction that ministers direct policy)
 *   - Formal Doctrine of Ministerial Responsibility: Institutional theater (institutional/arbitrage) — persists as invocation despite sporadic enforcement
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent institutional arrangement as an inescapable feature of accountability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_conventions__ministerial_responsibility, 0.52).
domain_priors:suppression_score(constitutional_conventions__ministerial_responsibility, 0.58).
domain_priors:theater_ratio(constitutional_conventions__ministerial_responsibility, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_conventions__ministerial_responsibility, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_conventions__ministerial_responsibility, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_conventions__ministerial_responsibility, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_conventions__ministerial_responsibility, tangled_rope).
narrative_ontology:human_readable(constitutional_conventions__ministerial_responsibility, "Ministerial Responsibility Convention").
narrative_ontology:topic_domain(constitutional_conventions__ministerial_responsibility, "political/legal/constitutional").

domain_priors:requires_active_enforcement(constitutional_conventions__ministerial_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_conventions__ministerial_responsibility, '5cd70fb2-025c-4a91-ba87-32ca4fc53ab6').
narrative_ontology:cs_kernel_codification('5cd70fb2-025c-4a91-ba87-32ca4fc53ab6', formalized).
narrative_ontology:cs_authority_grounding('5cd70fb2-025c-4a91-ba87-32ca4fc53ab6', lineage).
narrative_ontology:cs_interpretation_layer_present('5cd70fb2-025c-4a91-ba87-32ca4fc53ab6').
narrative_ontology:cs_reading_relation('5cd70fb2-025c-4a91-ba87-32ca4fc53ab6', constitutional_conventions__collective_responsibility, coexists_with).
narrative_ontology:cs_reading_relation('5cd70fb2-025c-4a91-ba87-32ca4fc53ab6', constitutional_conventions__royal_assent_convention, coexists_with).
narrative_ontology:cs_reading_relation('5cd70fb2-025c-4a91-ba87-32ca4fc53ab6', constitutional_conventions__salisbury_convention, coexists_with).
narrative_ontology:cs_axiom('5cd70fb2-025c-4a91-ba87-32ca4fc53ab6', foundational, individual_minister_answerable_to_parliament).
narrative_ontology:cs_axiom_status(individual_minister_answerable_to_parliament, holdable).
narrative_ontology:cs_axiom_grounding('5cd70fb2-025c-4a91-ba87-32ca4fc53ab6', individual_minister_answerable_to_parliament, deontological).
narrative_ontology:cs_axiom('5cd70fb2-025c-4a91-ba87-32ca4fc53ab6', foundational, bureaucratic_failure_requires_political_resignation).
narrative_ontology:cs_axiom_status(bureaucratic_failure_requires_political_resignation, holdable).
narrative_ontology:cs_axiom_grounding('5cd70fb2-025c-4a91-ba87-32ca4fc53ab6', bureaucratic_failure_requires_political_resignation, empirically_contingent).
narrative_ontology:cs_reference_frame('5cd70fb2-025c-4a91-ba87-32ca4fc53ab6', individual_accountability_framework).
narrative_ontology:cs_drift_state('5cd70fb2-025c-4a91-ba87-32ca4fc53ab6', contemporary_media_politics_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5cd70fb2-025c-4a91-ba87-32ca4fc53ab6', '').
narrative_ontology:cs_kernel_id(constitutional_conventions__ministerial_responsibility, constitutional_conventions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_conventions__ministerial_responsibility, parliamentary_commons).
narrative_ontology:constraint_beneficiary(constitutional_conventions__ministerial_responsibility, executive_leadership).
narrative_ontology:constraint_victim(constitutional_conventions__ministerial_responsibility, individual_ministers).
narrative_ontology:constraint_victim(constitutional_conventions__ministerial_responsibility, administrative_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SCAPEGOATED MINISTER (SNARE) — A minister bearing institutional failure with no genuine control over the anonymous bureaucracy below. Exit is resignation under pressure, but the underlying systemic failures persist. High suppression (career destruction, public humiliation) with minimal coordination benefit — the minister absorbs blame for structural problems they did not create and cannot unilaterally fix. This agent experiences pure extraction.
constraint_indexing:constraint_classification(constitutional_conventions__ministerial_responsibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL SERVICE HIERARCHY (TANGLED ROPE) — Benefits from the convention by remaining unaccountable: bureaucrats design and implement policy, but ministers bear public responsibility. The civil service gains a coordination benefit (ministerial delegation provides legitimacy cover) alongside an extraction benefit (shielded anonymity). Constrained exit because civil servants depend on the minister-as-buffer arrangement; if it collapsed, direct parliamentary scrutiny of bureaucracy would follow. Mixed coordination and extraction.
constraint_indexing:constraint_classification(constitutional_conventions__ministerial_responsibility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PARLIAMENTARY COMMONS (TANGLED ROPE) — Gains the ability to hold government accountable through a visible, resignable agent (the minister) rather than faceless bureaucracy. This is a genuine coordination benefit: the convention makes the executive answerable. However, the extraction cost is real: ministers often resign for failures they did not author or could not prevent, while the actual architects of failure remain sheltered. The Commons has mobile exit (can reform the convention, demand direct bureaucratic accountability) but chooses the easier path of extracting ministerial resignations. Mixed coordination and extraction from this perspective.
constraint_indexing:constraint_classification(constitutional_conventions__ministerial_responsibility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: PRIME MINISTER AND EXECUTIVE (ROPE) — Net beneficiary. The convention allows the Prime Minister to manage both Parliament (by providing resignations to satisfy scrutiny) and the bureaucracy (by maintaining civil service loyalty through the fiction that ministers, not officials, control policy). The executive experiences this as pure coordination: a mechanism for distributing accountability that protects the Prime Minister's authority while appearing to hold subordinates responsible. Arbitrage exit: the executive can reshape the convention (see Churchill's 1951 revival, or Thatcher's hardline) or enforce it strictly as needed.
constraint_indexing:constraint_classification(constitutional_conventions__ministerial_responsibility, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL DOCTRINE (PITON) — The stated doctrine of individual ministerial responsibility is largely theater. Actual resignations are rare and often voluntary rather than coerced; most ministers survive serious failures and are shuffled rather than ousted. The convention persists through ritual invocation ('ministers must resign for their departments') even though enforcement is sporadic and often inconsistent. High theater ratio reflects that the formal doctrine is invoked to legitimize decisions made on other grounds (political calculation, media pressure, party management). The doctrine's functional content has degraded.
constraint_indexing:constraint_classification(constitutional_conventions__ministerial_responsibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational/universal perspective, some form of individual accountability in governance is inherent to political legitimacy itself. You cannot have an answerable executive without a person to whom Parliament directs its scrutiny and from whom resignations flow. This view naturalizes ministerial responsibility as an inescapable feature of any accountable governance system. However, the structural data contradicts this: the beneficiary/victim split, the enforceability constraints, and the theater ratio reveal that the convention is a contingent institutional arrangement, not a natural law of politics.
constraint_indexing:constraint_classification(constitutional_conventions__ministerial_responsibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_conventions__ministerial_responsibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_conventions__ministerial_responsibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_conventions__ministerial_responsibility, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_conventions__ministerial_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_conventions__ministerial_responsibility, TR),
    TR >= 0.70.

:- end_tests(constitutional_conventions__ministerial_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The convention enables extraction of ministerial resignations (cost borne by individuals) to satisfy Parliament's accountability demand, while shielding the civil service from direct scrutiny. The extraction is substantial but not maximal because the convention also provides genuine coordination benefit: Parliament gains a mechanism to hold someone visibly accountable. Without ministerial responsibility, Parliament would face pure opacity (an unaccountable civil service). With it, Parliament gains accountability through a person. The cost is that the person may be a scapegoat. Suppression (0.58): Moderate-high. Ministers face high cost for refusing to resign when pressure is applied (career destruction, party expulsion, media condemnation). The suppression operates through political and institutional mechanisms, not legal ones — a minister cannot be compelled to resign, but the social cost is severe. Exit options for ministers are constrained: resignation is painful but exit, while remaining in government without resigning is political death. Theater ratio (0.65): Moderate-high. The doctrine states that ministers resign for serious failure, but enforcement is inconsistent. Many ministers survive major failures (Profumo was rare; most post-2000 scandals result in reshuffles, not resignations). The ritual is invoked (the Prime Minister 'accepts' a resignation with performative regret) even when the resignation was politically calculated rather than accountability-driven. Over the 40-year interval, theater ratio has risen as media coverage has become more performative and political calculation more transparent.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a deep perspectival split. The scapegoated minister (powerless/trapped) sees pure extraction — they bear blame for structural failures. The civil service (powerful/constrained) sees coordination benefit plus extraction: they gain anonymity while officials authorize policy. The Commons (organized/mobile) sees genuine coordination benefit: they have a person to scrutinize. The Prime Minister (institutional/arbitrage) sees pure coordination: the convention lets them manage both Parliament and bureaucracy. The formal doctrine (institutional/arbitrage) is theater: the stated rule (resign for serious failure) is invoked unevenly. The analytical observer risks seeing a natural law (accountability requires an answerable person) when the data reveals a contingent arrangement (this particular way of organizing accountability is one option among others).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values vary by structural position. The scapegoated minister (powerless/trapped/victim) experiences maximum d ≈ 0.95 (full target of extraction). The civil service (powerful/constrained/beneficiary) derives d from the tension between anonymity benefit and constrained exit, roughly d ≈ 0.30 (net beneficiary but with costs). The Commons (organized/mobile/mixed) derives d from scrutiny benefit but acceptance of ministerial fig-leafing, roughly d ≈ 0.55 (symmetric or slight victim). The Prime Minister (institutional/arbitrage/beneficiary) derives d ≈ 0.05 (net beneficiary with arbitrage flexibility). The analytical observer derives d ≈ 0.72 (observational position). These variations explain why the same constraint classifies as Snare from the minister's view, Rope from the PM's, Tangled Rope from the Commons', and Mountain from the analytical view that risks naturalizing the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that ministerial responsibility is a genuine Tangled Rope constraint with real coordination benefit (Parliament can scrutinize an answerable agent) alongside real extraction (ministers bear costs for bureaucratic failures they did not author). The constraint is NOT pure extraction (Snare) because the coordination mechanism is functionally real — without it, Parliament would face complete opacity. It is NOT pure coordination (Rope) because the extraction of ministerial resignations is substantial and asymmetric. The rise in theater ratio (0.45 to 0.65 over 40 years) suggests the coordination function is degrading relative to the extractive function — more resignations are political theater, fewer are accountability-driven. This tracks the rising power of media cycles and political calculation relative to genuine parliamentary scrutiny. The Tangled Rope classification holds at the current endpoint (ε = 0.52) but the trajectory suggests drift toward Piton (high theater, degraded function) or Snare (rising extraction, declining coordination). The mandatrophy is not whether to reclassify now, but to recognize that the reading's internal stability is weakening.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resignation_causation_ambiguity,
    'Does a minister''s resignation reflect genuine accountability for departmental failure, or is it a political theater move by the Prime Minister to satisfy Parliament while protecting the civil service?',
    'Temporal analysis: track whether resignations follow performance failures (accountability model) or political events like budget crises or media scandals unrelated to ministerial competence (theater model). Correlate resignation timing with failure severity and minister culpability.',
    'If resignations track actual failure: the convention has real coordination content (Rope from Commons perspective). If resignations are political theater: the convention is extraction mechanism (Snare from minister perspective). Current evidence suggests mixed: some resignations are genuine accountability (Crichel Down precedent), most are political theater (ministers survive massive failures).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resignation_causation_ambiguity, empirical, 'Whether resignations reflect genuine accountability or political theater').

omega_variable(
    civil_service_anonymity_mechanism,
    'Is the anonymity of the civil service a feature of ministerial responsibility (enabling coordination by shielding permanent service from political turmoil) or a bug (enabling unaccountable bureaucratic extraction)?',
    'Comparative constitutional analysis: compare UK outcomes where civil service remains anonymous with systems where bureaucrats face direct parliamentary scrutiny (Germany, Sweden) or public accountability (US). Measure policy quality, responsiveness, and corruption rates.',
    'If anonymity enables better coordination: ministerial responsibility is Rope (protects civil service, enables continuity). If anonymity enables extraction: it is Snare (protects bureaucratic failure). If both: it is Tangled Rope (genuine coordination benefit + extraction mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_service_anonymity_mechanism, empirical, 'Whether civil service anonymity serves coordination or enables extraction').

omega_variable(
    collective_vs_individual_reading_foreclosure,
    'Does the doctrine of individual ministerial responsibility logically foreclose the doctrine of collective responsibility, or can both be held simultaneously?',
    'Doctrinal analysis: Examine whether a minister can simultaneously be individually accountable for their department AND collectively responsible for Cabinet decisions they opposed. Current practice: both doctrines are invoked. Identify whether they are compatible or whether invoking one rules out the other in principled analysis.',
    'If they foreclose each other: ministerial_responsibility and collective_responsibility are incompatible readings of the same kernel, and the constitution cannot hold both. If they coexist: they represent different aspects of the same accountability structure. Current doctrine treats them as coexisting but inconsistent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_vs_individual_reading_foreclosure, conceptual, 'Whether individual and collective ministerial responsibility are logically compatible').

omega_variable(
    convention_enforceability_degradation,
    'Has the convention''s enforcement mechanism degraded over time, with ministers surviving failures that would have triggered resignation in earlier periods?',
    'Historical analysis: compare resignation triggers and rates across decades. Correlate with media environment (24-hour news cycle, social media amplification), parliamentary capacity (number of select committees, scrutiny depth), and political polarization.',
    'If enforcement has degraded: the convention is transitioning from Rope (coordination) toward Piton (inertial theater). This would suggest the ministerial responsibility reading is losing coherence and competitors (collective responsibility, direct bureaucratic accountability) may gain ground.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(convention_enforceability_degradation, empirical, 'Whether the convention''s enforcement has degraded over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_conventions__ministerial_responsibility, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(minresp_tr_t0, constitutional_conventions__ministerial_responsibility, theater_ratio, 0, 0.45).
narrative_ontology:measurement(minresp_tr_t20, constitutional_conventions__ministerial_responsibility, theater_ratio, 20, 0.58).
narrative_ontology:measurement(minresp_tr_t40, constitutional_conventions__ministerial_responsibility, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(minresp_be_t0, constitutional_conventions__ministerial_responsibility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(minresp_be_t20, constitutional_conventions__ministerial_responsibility, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(minresp_be_t40, constitutional_conventions__ministerial_responsibility, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(minresp_su_t0, constitutional_conventions__ministerial_responsibility, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(minresp_su_t20, constitutional_conventions__ministerial_responsibility, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(minresp_su_t40, constitutional_conventions__ministerial_responsibility, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_conventions__ministerial_responsibility, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_conventions__ministerial_responsibility, constitutional_conventions__collective_responsibility).
narrative_ontology:affects_constraint(constitutional_conventions__ministerial_responsibility, constitutional_conventions__royal_assent_convention).
narrative_ontology:affects_constraint(constitutional_conventions__ministerial_responsibility, constitutional_conventions__salisbury_convention).

% DUAL FORMULATION NOTE:
% Ministerial responsibility is one reading of a contested constitutional kernel. The four constraint stories (ministerial_responsibility, collective_responsibility, royal_assent_convention, salisbury_convention) represent different interpretations of how accountability flows through the constitution. All four are instantiations of the same underlying kernel: the stabilized commitment that the UK constitution is an uncodified set of enforced conventions. Ministerial responsibility focuses on individual minister accountability; collective responsibility focuses on cabinet unity; royal assent focuses on the crown's formal (but unused) veto; Salisbury focuses on the Lords' self-denying ordinance. Each story has its own ε value and perspectival structure, but they are linked by the shared kernel and mutual influence through constitutional interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_conventions__ministerial_responsibility, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
