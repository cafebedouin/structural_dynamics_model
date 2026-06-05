% ============================================================================
% CONSTRAINT STORY: balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balancing_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: balancing_reading
 *   human_readable: Balancing Reading of Speech Protection and Competing Interests
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The balancing reading of the speech protection boundary is one
 *   interpretive choice among contested alternatives for how constitutional
 *   law should protect freedom of speech while accommodating competing
 *   interests in dignity, equality, public safety, and community welfare.
 *   This reading holds that no single right categorically overrides others;
 *   rather, courts must engage in case-by-case balancing that weighs speech
 *   interests against legitimate governmental objectives. The reading
 *   generates a distinctive structural constraint: it institutionalizes
 *   judicial discretion, creates variable victim sets depending on balancing
 *   outcomes, and produces suppression that is context-dependent rather than
 *   categorical. From the perspective of judicial institutions and executive
 *   actors, balancing doctrine functions as pure coordination — courts and
 *   governments solve the genuine problem of adjudicating competing interests
 *   without absolute categorical rules. From the perspective of speech
 *   claimants and marginalized communities, the same doctrine functions as
 *   Snare or Tangled Rope: the absence of categorical protection means
 *   continuous litigation, burden-shifting, and the risk that balancing will
 *   discount their interests. The constraint exhibits the hallmark signature
 *   of a reading choice rather than a natural law: beneficiaries (judicial
 *   institutions) experience it as coordination, while victims (speech
 *   claimants, predictability of law) experience it as extraction. The
 *   theater ratio has increased over the interval (0.45→0.68) as balancing
 *   doctrine has accumulated layers of doctrinal refinement (strict scrutiny,
 *   intermediate scrutiny, rational basis, compelling interest tests, narrow
 *   tailoring) that are largely performative — courts apply these formulas
 *   without constraint on outcomes. Extractiveness has risen from 0.35 to
 *   0.58 as balancing discretion has expanded and the number of recognized
 *   competing interests (dignity, security, equality, public health, cultural
 *   sensitivity) has grown.
 *
 * KEY AGENTS:
 *   - Speech Claimants: Primary victims (powerless/trapped) — must defend speech rights case-by-case through costly litigation with no categorical protection; bear full burden of balancing
 *   - Judicial Institutions: Primary beneficiaries (institutional/arbitrage) — exercise delegated authority to adjudicate; can shift balancing weights without changing doctrine; experience constraint as pure coordination
 *   - Marginalized Communities: Secondary victims (moderate/constrained) — experience dual extraction: balancing doctrine can justify suppression of their speech, yet it remains their only constitutional tool to challenge censorship
 *   - Civil Society / Free Speech Advocacy: Organized victims (organized/constrained) — face constant litigation burden and unpredictable outcomes; cannot establish clear standards for policy reform
 *   - Executive and Legislative Actors: Secondary beneficiaries (institutional/arbitrage) — retain regulatory flexibility to justify speech restrictions on case-by-case basis; benefit from lack of categorical prohibition
 *   - Analytical Observer: Risk of naturalizing reading choice as immutable law (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balancing_reading, 0.58).
domain_priors:suppression_score(balancing_reading, 0.62).
domain_priors:theater_ratio(balancing_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balancing_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(balancing_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(balancing_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balancing_reading, tangled_rope).
narrative_ontology:human_readable(balancing_reading, "Balancing Reading of Speech Protection and Competing Interests").
narrative_ontology:topic_domain(balancing_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balancing_reading, 'd564b190-7c2f-4715-916c-54cfd3750687').
narrative_ontology:cs_created_at('d564b190-7c2f-4715-916c-54cfd3750687', '').
narrative_ontology:cs_kernel_codification('d564b190-7c2f-4715-916c-54cfd3750687', formalized).
narrative_ontology:cs_authority_grounding('d564b190-7c2f-4715-916c-54cfd3750687', lineage).
narrative_ontology:cs_interpretation_layer_present('d564b190-7c2f-4715-916c-54cfd3750687').
narrative_ontology:cs_kernel_id(balancing_reading, speech_protection_boundary).
narrative_ontology:cs_reading_relation('d564b190-7c2f-4715-916c-54cfd3750687', near_absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d564b190-7c2f-4715-916c-54cfd3750687', dignitary_harm_reading, influences).
narrative_ontology:cs_axiom('d564b190-7c2f-4715-916c-54cfd3750687', foundational, speech_and_competing_interests_incommensurable).
narrative_ontology:cs_axiom_status(speech_and_competing_interests_incommensurable, holdable).
narrative_ontology:cs_axiom('d564b190-7c2f-4715-916c-54cfd3750687', foundational, categorical_rules_inadequate_constitutional_interpretation).
narrative_ontology:cs_axiom_status(categorical_rules_inadequate_constitutional_interpretation, holdable).
narrative_ontology:cs_reference_frame('d564b190-7c2f-4715-916c-54cfd3750687', flexible_adjudication_framework).
narrative_ontology:cs_drift_state('d564b190-7c2f-4715-916c-54cfd3750687', contemporary_expanded_competing_interests, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balancing_reading, judicial_institutions).
narrative_ontology:constraint_beneficiary(balancing_reading, executive_discretion).
narrative_ontology:constraint_victim(balancing_reading, speech_claimants).
narrative_ontology:constraint_victim(balancing_reading, predictability_of_law).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SPEECH CLAIMANT (SNARE) — Individual asserting speech rights faces maximum extractive burden: must litigate case-by-case through costly judicial processes with no categorical protection, bearing full suppression cost of balancing doctrine. No alternative forum; no exit from the requirement to defend speech in each instance.
constraint_indexing:constraint_classification(balancing_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITIES (TANGLED ROPE) — Experience dual structure: balancing doctrine can justify suppression of their speech (dignitary harm, public safety arguments disproportionately apply to minority voices) but also provides the only tool to challenge governmental censorship. Coordination function (ability to bring constitutional challenge) exists alongside asymmetric extraction (burden of proof and discounting of their interests in balancing calculus).
constraint_indexing:constraint_classification(balancing_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIAL INSTITUTIONS (ROPE) — Beneficiaries of balancing doctrine. Courts experience the constraint as pure coordination: balancing doctrine delegates authority to adjudicate and preserves judicial discretion over categorical rules. No suppression from court's perspective — they exercise power rather than having it constrained. Arbitrage exit: courts can shift balancing weights without changing doctrine.
constraint_indexing:constraint_classification(balancing_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EXECUTIVE AND LEGISLATIVE ACTORS (ROPE) — Secondary beneficiaries. Balancing doctrine enables regulatory flexibility: government retains latitude to justify speech restrictions (public safety, security, dignitary harm) on a case-by-case basis without categorical prohibition. Experienced as coordination mechanism for governance.
constraint_indexing:constraint_classification(balancing_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL SOCIETY (TANGLED ROPE) — Organized actors see balancing doctrine as institutionalizing the constraint they aim to challenge. The doctrine enables coordination (they can invoke constitutional framework) but extracts through unpredictability: without categorical rules, their advocacy work requires constant litigation, resource diversion, and they cannot establish clear standards for policy reform. Constrained exit because the balancing framework is the only available constitutional tool.
constraint_indexing:constraint_classification(balancing_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of false summit: claims that balancing is inherent to constitutional interpretation — that it is logically necessary to weigh competing interests, that categorical speech rules are impossible without destroying the state, that context always matters. This naturalizes what is actually a reading choice with contingent institutional consequences.
constraint_indexing:constraint_classification(balancing_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balancing_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(balancing_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(balancing_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The balancing reading institutionalizes judicial discretion in a way that extracts substantial value for courts and executive actors. Speech claimants must litigate each restriction individually; no categorical protection shields them from case-by-case scrutiny. The extractiveness is not extreme (0.58 rather than 0.72+) because the doctrine does provide a constitutional framework within which speech can be challenged — there is a coordination function (courts can hear constitutional arguments) alongside the extraction. The rise in extractiveness over the interval (0.35→0.58) reflects accumulation of competing interests (security, dignity, equality) that have enlarged the space in which speech can be limited through balancing. Suppression (0.62): Moderate-high. Significant barriers to speech protection include: (1) high litigation costs; (2) unpredictability of outcomes due to discretionary balancing; (3) burden placed on speech claimants to prove their interest outweighs competing factors; (4) asymmetric application — marginalized speakers' interests may be systematically discounted. However, suppression is not total (not 0.85+) because the constitutional framework exists and courts can and do protect speech when convinced the balance favors it. Theater ratio (0.68): High and rising. Balancing doctrine has accumulated layers of performative doctrinal refinement: strict scrutiny, intermediate scrutiny, narrow tailoring, compelling interest tests. These formulas structure judicial reasoning but do not constrain outcomes. Courts can apply the formulas while reaching ideologically or politically convenient conclusions. The rise in theater ratio (0.45→0.68) reflects this accumulation: the doctrine has become more elaborate without becoming more predictable or constraining.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is the clearest diagnostic signature of this reading as a choice rather than a natural law. Judicial institutions (institutional/arbitrage) classify the constraint as pure Rope — they experience balancing as solving the legitimate coordination problem of adjudicating speech claims without categorical prohibition. Courts gain authority and discretion; they experience no suppression because they exercise power rather than having it constrained. Speech claimants (powerless/trapped) classify the constraint as Snare — they face maximum extraction because they must litigate every restriction individually, bearing all costs and risks of the balancing process. Marginalized communities (moderate/constrained) classify the constraint as Tangled Rope because it is genuinely mixed: the balancing framework enables them to bring constitutional challenges (coordination function) but creates systematic risk that their interests will be discounted in balancing (extraction). Civil society organizations (organized/constrained) also see Tangled Rope: they can organize around constitutional framework but face constant unpredictability. The analytical observer risks classifying as Mountain — claiming balancing is inherent to constitutional interpretation — but the structural data reveals this as a false summit: beneficiaries (courts) benefit from the framing, making it a reading choice rather than a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) tracks each agent's structural position relative to extraction flow. Judicial institutions benefit from balancing discretion: d ≈ 0.05 (beneficiary with full arbitrage exit). Speech claimants bear the burden: d ≈ 0.92 (victim with trapped exit). Marginalized communities occupy middle ground: d ≈ 0.65 (mixed — they are both victims of potential suppression and beneficiaries of the constitutional framework as a challenge tool). Civil society organizations constrained by litigation burden: d ≈ 0.72 (high but not maximal because they retain some agency and exit capacity through political pressure for reform). The signature sigmoid f(d) transformation means that powerless/trapped agents experience the highest effective extraction (f(0.92)≈1.35), while beneficiaries with arbitrage exit experience negative effective extraction (f(0.05)≈-0.11), meaning the constraint subsidizes them. The perspectival gap (different d values → different experiences of χ) is the analytical evidence that balancing is a reading choice: the same structural constraint looks like Rope to high-beneficiary agents and Snare to high-burden agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The balancing reading resolves mandatrophy by showing how the same constitutional kernel (speech protection boundary) generates different constraints depending on which reading agents adopt. The balancing reading produces high extractiveness (0.58) because it institutionalizes discretion; a near-absolutist reading would produce lower extractiveness (0.15-0.25) by placing categorical constraints on balancing; a dignitary-harm reading would produce different victim sets and suppression profiles. The reading choice is not about empirical fact (whether balancing 'really' happens) but about constitutional legitimacy: is the framework that grants courts discretion to balance interests, or is it a framework that places categorical constraints on that discretion? The mandatrophy is resolved not by proving one reading 'correct' but by recognizing that each reading instantiates structurally distinct constraints with different extractiveness, victim sets, and beneficiary profiles. The balancing reading's extractiveness (0.58) and theater ratio (0.68) are evidence that it produces higher institutional discretion and more performative adjudication than categorical alternatives would.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_content_determination,
    'How are balancing weights determined? What makes one interest outweigh speech in a given context, and who decides?',
    'Historical analysis of Supreme Court balancing tests (strict scrutiny, intermediate scrutiny, rational basis); pattern analysis of which classes of interests (security, dignity, equality, public health) received what weight across time periods and political composition of courts',
    'If weights are principled and predictable: constraint moves toward Rope (coordination function predominates). If weights track judicial ideology or political pressure: constraint remains Snare (extractive discretion). If weights have shifted over time: reveals whether balancing masks institutional capture or reflects genuine doctrinal development.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(balancing_content_determination, empirical, 'Whether balancing weights are principled or discretionary').

omega_variable(
    categorical_rule_feasibility,
    'Could categorical rules protecting speech (near-absolutist reading) function at scale without collapsing state capacity? Or is balancing logically necessary?',
    'Comparative constitutional law analysis: jurisdictions with categorical rules (some European speech protections, Canadian harm thresholds) and outcomes; game-theoretic analysis of what happens at the boundary if categorical rules block security/equality interventions',
    'If categorical rules are empirically feasible: balancing reading appears contingent rather than necessary (forecloses reading relation becomes possible). If categorical rules create genuine state dysfunction: balancing is not a choice but structural necessity (coexists_with or influences relation more stable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_rule_feasibility, empirical, 'Whether categorical speech rules could function as legal framework').

omega_variable(
    reading_founder_versus_interpreter_gap,
    'Did the constitutional text intend balancing (weighing competing interests) or did balancing emerge as an interpretive choice by later judicial actors?',
    'Historical analysis of Framers'' intent documents, contemporary speech law in 1791; comparison with explicit balancing language in post-1960s Supreme Court doctrine; identification of turning point where categorical prohibition shifted to balancing framework',
    'If balancing is original intent: reading is grounded in founder commitment (authority_grounding=lineage stable). If balancing is judicial overlay: reading represents interpretive drift from original kernel (drift_state=axiom_overriding or practice_drift).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_founder_versus_interpreter_gap, conceptual, 'Whether balancing reflects constitutional text or judicial innovation').

omega_variable(
    false_summit_risk,
    'Is balancing doctrine a natural law of constitutional interpretation or a reading choice that naturalizes institutional beneficiaries?',
    'Cross-reading comparison: empirical differences in constraint outcome (extractiveness, suppression, victim set) when near-absolutist or dignitary readings are applied to the same cases; identification of which institutional actors benefit from balancing framing',
    'If balancing is necessary: mountain classification is accurate. If balancing is choice: mountain is false summit, constraint reclassifies as Tangled Rope with beneficiaries (judicial institutions, executive discretion) that depend on balancing framing to maintain extracted value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_risk, conceptual, 'Whether balancing naturalizes institutional beneficiaries').

omega_variable(
    minority_voice_systematic_loss,
    'Does balancing doctrine systematically discount marginalized voices'' interests in the balance, or does it merely create variable outcomes?',
    'Empirical analysis: coding of Supreme Court balancing opinions by race, gender, political position of speech claimant; statistical analysis of whether marginalized groups'' interests receive lower weight in balancing calculus; identification of whether dignitary harm arguments are invoked asymmetrically against minority speech',
    'If systematic discounting: constraint converts Tangled Rope into effective Snare from marginalized perspective (integration into doctrinal framework masks unequal weighting). If variable but symmetric: Tangled Rope classification stable (genuine mixed coordination-extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minority_voice_systematic_loss, empirical, 'Whether balancing systematically disadvantages marginalized speakers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balancing_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bala_tr_t0, balancing_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(bala_tr_t15, balancing_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(bala_tr_t30, balancing_reading, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(bala_be_t0, balancing_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bala_be_t15, balancing_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(bala_be_t30, balancing_reading, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(balancing_reading, near_absolutist_reading).
narrative_ontology:affects_constraint(balancing_reading, dignitary_harm_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_boundary kernel decomposes into three structurally distinct constraints via three readings: balancing_reading (ε=0.58), near_absolutist_reading (ε≈0.18), dignitary_harm_reading (ε≈0.45). Each reading generates different victim sets, beneficiary groups, and extractiveness profiles. They are not three perspectives on one constraint; they are three constraints on one kernel. The network edges represent that balancing_reading influences both siblings through its institutional effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
