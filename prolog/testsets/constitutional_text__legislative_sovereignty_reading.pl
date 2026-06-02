% ============================================================================
% CONSTRAINT STORY: constitutional_text__legislative_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__legislative_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__legislative_sovereignty_reading
 *   human_readable: Constitutional Text—Legislative Sovereignty Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The legislative sovereignty reading treats constitutional text as a
 *   kernel whose meaning is ultimately determined by parliamentary will,
 *   constrained only by the text's letter and subject to override through
 *   notwithstanding clauses, simple legislative majorities, or constitutional
 *   amendment procedures that require only parliamentary supermajority. This
 *   reading instantiates one corner of a contested constitutional space:
 *   other readings locate final authority in courts (judicial supremacy) or
 *   in 'the people' (popular sovereignty). The legislative sovereignty
 *   reading is prominent in Westminster traditions (UK, Canada, Australia)
 *   and in parliamentary constitutional systems generally. It grounds
 *   legitimacy in the principle that an elected body accountable to voters
 *   should have final say on constitutional meaning rather than an appointed
 *   judiciary. The constraint exhibits the signature of a tangled rope:
 *   genuine coordination function (the reading provides a framework for
 *   stable, predictable constitutional interpretation rooted in democratic
 *   accountability) combined with asymmetric extraction (minorities cannot
 *   exit; legislative majorities can override protections at will). The
 *   measurements show a modest upward drift in both theater ratio and
 *   extractiveness over the 20-year interval, reflecting judicial power
 *   increasing in practice while supremacy doctrine persists in form—a piton
 *   signature embedded within the tangled rope.
 *
 * KEY AGENTS:
 *   - Majoritarian Legislative Coalition: Primary beneficiary (organized/arbitrage) — captures ultimate authority over constitutional meaning; exercises full agency
 *   - Minority Rights Bearer: Primary victim (powerless/trapped) — protected only at majoritarian sufferance; cannot exit national legal framework
 *   - Judicial Branch: Secondary actor (institutional/constrained) — constrained by override power but benefits from coordinating constitutional interpretation
 *   - Constitutional Text as Stabilizing Kernel: Structural actor (institutional/constrained) — provides coordination function but is simultaneously subordinated by supremacy principle
 *   - Parliamentary Tradition and Institutional Memory: Institutional inertia (institutional/arbitrage) — maintains supremacy theater despite practical power shifts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.58).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.65).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Constitutional Text—Legislative Sovereignty Reading").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, 'c7ec9dc1-a4d1-4669-b66e-b752b3963677').
narrative_ontology:cs_kernel_codification('c7ec9dc1-a4d1-4669-b66e-b752b3963677', formalized).
narrative_ontology:cs_authority_grounding('c7ec9dc1-a4d1-4669-b66e-b752b3963677', lineage).
narrative_ontology:cs_interpretation_layer_present('c7ec9dc1-a4d1-4669-b66e-b752b3963677').
narrative_ontology:cs_reading_relation('c7ec9dc1-a4d1-4669-b66e-b752b3963677', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('c7ec9dc1-a4d1-4669-b66e-b752b3963677', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('c7ec9dc1-a4d1-4669-b66e-b752b3963677', foundational, elected_bodies_final_constitutional_authority).
narrative_ontology:cs_axiom_status(elected_bodies_final_constitutional_authority, holdable).
narrative_ontology:cs_axiom_grounding('c7ec9dc1-a4d1-4669-b66e-b752b3963677', elected_bodies_final_constitutional_authority, deontological).
narrative_ontology:cs_axiom('c7ec9dc1-a4d1-4669-b66e-b752b3963677', foundational, judicial_review_advisory_not_binding).
narrative_ontology:cs_axiom_status(judicial_review_advisory_not_binding, overridden).
narrative_ontology:cs_axiom_grounding('c7ec9dc1-a4d1-4669-b66e-b752b3963677', judicial_review_advisory_not_binding, conventional).
narrative_ontology:cs_reference_frame('c7ec9dc1-a4d1-4669-b66e-b752b3963677', parliamentary_sovereignty_tradition).
narrative_ontology:cs_drift_state('c7ec9dc1-a4d1-4669-b66e-b752b3963677', contemporary_judicial_influence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c7ec9dc1-a4d1-4669-b66e-b752b3963677', '').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, majoritarian_coalition).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, legislative_authority).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, minority_rights_protection).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, constitutional_rigidity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY RIGHTS BEARER (SNARE) — Structurally trapped by majoritarian override power. Legislative supremacy via notwithstanding clauses creates a trap: constitutional protections exist only at majoritarian sufferance. Cannot exit national legal framework; bears full extraction risk when majorities change. Maximum experienced extraction.
constraint_indexing:constraint_classification(constitutional_text__legislative_sovereignty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: JUDICIAL BRANCH (TANGLED ROPE) — Constrained by legislative override power but also benefits from coordinating constitutional meaning through advisory function. Courts preserve institutional prestige and interpretive influence (even if non-binding) while facing real suppression: their constitutional readings can be overridden at will. Mixed experience of coordination (judicial role in meaning-making) and extraction (powerlessness to bind outcomes).
constraint_indexing:constraint_classification(constitutional_text__legislative_sovereignty_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJORITARIAN LEGISLATIVE COALITION (ROPE) — Benefits from coordination function: legislative supremacy enables stable governance without deadlock via rigorous constitutional amendment procedures. No suppression experienced; full agency and exit options (can amend constitution, override courts, rewrite laws). Net beneficiary. Pure coordination from this perspective.
constraint_indexing:constraint_classification(constitutional_text__legislative_sovereignty_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PARLIAMENTARY TRADITION (PITON) — The supremacy doctrine persists through institutional and rhetorical commitment to parliamentary sovereignty, but the functional role has degraded: in practice, courts exercise substantial interpretive power despite formal subordination; legislatures often defer to judicial reasoning; constitutional override powers exist but are rarely invoked. The supremacy theater is maintained through formal doctrine while actual power has shifted. Theater ratio high; extractiveness lower than the formal rule suggests.
constraint_indexing:constraint_classification(constitutional_text__legislative_sovereignty_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL TEXT AS STABILIZING KERNEL (TANGLED ROPE) — From the perspective of the constitutional text itself as a coordination object: the text provides genuine coordination function (shared reference for legitimacy, dispute resolution framework) but is simultaneously undermined by the legislative override power embedded within it. The supremacy clause guarantees legislative extraction from the text's authority. Coordination + extraction hybrid.
constraint_indexing:constraint_classification(constitutional_text__legislative_sovereignty_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, legislative supremacy in constitutional meaning may appear as a natural law: any living legal system must ultimately trace authority to a sovereign body that can enforce its interpretations; courts cannot bind a legislature without that legislature's consent; therefore supremacy must rest with the branch that controls enforcement. But this classification is a false summit — it naturalizes a choice about sovereignty location, not a law of nature.
constraint_indexing:constraint_classification(constitutional_text__legislative_sovereignty_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__legislative_sovereignty_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_text__legislative_sovereignty_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_text__legislative_sovereignty_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, TR),
    TR >= 0.70.

:- end_tests(constitutional_text__legislative_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The legislative supremacy reading centralizes authority in a majoritarian body without rigorous amendment procedures to bind future legislatures. This creates substantial extraction risk for minorities: their constitutional protections depend on majoritarian sufferance and can be overridden through ordinary legislative procedures (notwithstanding clauses) or ordinary-level constitutional amendments (in some jurisdictions). However, extractiveness is not maximal (0.72+) because: (a) coordination function is genuine—the reading provides a clear framework for stable governance without gridlock; (b) override powers, while real, are not costless—legislative action is visible and subject to electoral consequence; (c) some jurisdictions pair supremacy with procedural protections (supermajority requirements, referenda) that raise the cost of override. Suppression (0.65): Moderate-high. Minorities face strong suppression: they cannot exit the legal system, cannot block majoritarian will through judicial challenge (courts are advisory only), and cannot prevent constitutional override through formal procedures. However, suppression is not total (0.85+) because: (a) electoral mechanisms provide some voice (minorities can campaign for different majorities); (b) minority coalitions can sometimes block supermajorities in legislatures; (c) procedural visibility creates political cost to override. Theater ratio (0.48): Moderate-low. The legislative supremacy reading has relatively low theater compared to other constitutional doctrines—formal and practical authority largely align. Where theater rises (from 0.35 to 0.48 over the interval) is in the gap between the supremacy doctrine and actual judicial power: courts exercise substantial interpretive influence while the doctrine formally subordinates them. This gap is the piton signature.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a profound perspectival gap between the majoritarian legislative coalition (Rope—pure coordination) and the minority rights bearer (Snare—pure extraction). Both perspectives read the same constitutional text, the same institutional arrangement, but experience opposite extractions. The legislative coalition experiences the supremacy reading as enabling stable, accountable governance: the text coordinates expectations, courts provide useful interpretation, legislatures retain final authority (which they rarely need to invoke). The minority rights bearer experiences the same arrangement as a trap: constitutional protections are legally hollow because they can be overridden at will. The gap is not empirical disagreement but structural difference in exit options and power—the minority has neither. The judicial branch occupies the middle (tangled rope): constrained by supremacy doctrine but also benefiting from interpretive role. The parliamentary tradition perspective (piton) reveals that the gap between doctrine and practice has widened over time—courts now exercise substantial power that supremacy formally denies them, yet the doctrine persists because legislatures have not invoked overrides frequently enough to make the subordination salient.
 *
 * DIRECTIONALITY LOGIC:
 *   Legislative supremacy locates all constitutional authority in the elected body. The derivation chain produces: Majoritarian Coalition = beneficiary + arbitrage exit → d≈0.10 → f(d)≈-0.05 → effective extraction negative (coordination benefit). Minority Rights Bearer = victim + trapped exit → d≈0.95 → f(d)≈1.42 → maximum effective extraction. Judicial Branch = constrained exit + split role (partially victim of supremacy, partially beneficiary of interpretive role) → d≈0.55 → f(d)≈0.75 → moderate extraction. Constitutional Text = victim of subordination + constrained role → d≈0.65 → f(d)≈1.00 → moderate extraction. Parliamentary Tradition = beneficiary via doctrinal continuity + arbitrage exit → d≈0.20 → f(d)≈0.02 → near-zero effective extraction. Analytical Observer = analytical exit → canonical d≈0.73 → f(d)≈1.15 → moderate effective extraction (sees both coordination and extraction). The overrides are positioned at the constraints declared in base_properties: majoritarian_coalition and legislative_authority as beneficiaries (they derive positive from the reading); minority_rights_protection and constitutional_rigidity as victims (they derive extraction from the reading's subordination logic).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through kernel decomposition. A single natural-language concept ('constitutionalism,' 'the rule of law,' 'constitutional constraint on power') decomposes into three structurally distinct constraints: legislative supremacy (ε=0.58), judicial supremacy (ε would differ, likely ≤0.40 with beneficiary shift to judicial institution), and popular sovereignty (ε would differ again). The mandate is not 'which reading is correct'—all three are live in current constitutional practice—but 'which structural claim are you making about authority location?' This reading makes the claim that elected legislatures should have final say on constitutional meaning, which generates a snare for minorities, coordination for legislators, and extraction for minority protections. No single type resolves mandatrophy; the presheaf of readings over the constitutional kernel does.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legislative_override_frequency_threshold,
    'How frequently must override powers be invoked for legislative supremacy to be structurally real rather than theoretically dormant?',
    'Historical analysis of actual override/notwithstanding invocations vs. total years of constitutional operation; correlation between threat of override and judicial deference patterns',
    'High invocation (>1% annually): supremacy is active extraction mechanism (Snare confirmed). Low invocation (<0.1% annually): supremacy is latent threat that shapes behavior through deterrence rather than extraction (classification shifts toward Rope). Dormancy (<0.01% annually): supremacy becomes purely formal; actual coordination via mutual deference (Rope or Mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legislative_override_frequency_threshold, empirical, 'Frequency of legislative override invocation determining structural reality of supremacy').

omega_variable(
    judicial_deference_causality,
    'Does judicial deference to legislative judgment arise from formal subordination (supremacy doctrine) or from independent institutional roles and expertise deference?',
    'Comparative analysis across jurisdictions: systems with formal legislative supremacy vs. systems with explicit judicial co-equality; correlation between doctrinal supremacy claims and actual judicial behavior; analysis of judicial opinions citing supremacy doctrine vs. those grounding deference in institutional role differentiation',
    'If deference is caused by supremacy doctrine: removing the doctrine would shift power (validates extraction reading). If deference is caused by role differentiation: supremacy doctrine is ornamental (validates piton/mountain reading). If both mechanisms operate: decompose into separate constraints (ε-invariance principle).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_deference_causality, empirical, 'Whether judicial deference is caused by supremacy doctrine or institutional role distinction').

omega_variable(
    reading_foreclosure_status,
    'Does legislative supremacy reading logically foreclose the judicial supremacy reading within a single constitutional framework, or can both coexist as different institutional readings?',
    'Logical analysis of core premises: if legislative supremacy is defined as ''legislature has final say on meaning,'' does that exclude ''courts have final say''? Or can both readings hold simultaneously as empirical claims about who actually decides in practice? Historical analysis of jurisdictions attempting both readings (Canada, UK).',
    'If forecloses: the readings are competitors for a single authoritative position (rare—validates forecloses relation). If coexist: the readings partition institutional domains (validates coexists_with). If both are partial descriptions: decompose into observational constraints for each domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_status, conceptual, 'Whether legislative supremacy logically forecloses judicial supremacy reading').

omega_variable(
    constitutional_amendment_constraint_interaction,
    'If rigorous amendment procedures exist, do they constitute a separate constraint (constitutional amendment as Rope or Scaffold) or are they embedded within the legislative supremacy constraint?',
    'Analysis of amendment procedure invocation rates, success rates, and institutional barriers relative to override procedures. If amendment procedures function as a stabilizing alternative to override (creating exit option for constitutional change), decompose into separate constraint.',
    'If decomposed: legislative supremacy extractiveness may be lower (exit via amendment reduces suppression). If embedded: amendment procedures are part of the supremacy mechanism (unchanged extractiveness). Affects victim group definition (constitutional stability actors become beneficiaries if amendment is an exit option).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_amendment_constraint_interaction, empirical, 'Whether amendment procedures constitute separate constraint or component of supremacy mechanism').

omega_variable(
    practical_power_distribution_vs_formal_doctrine,
    'In practice, do courts or legislatures exercise greater power over constitutional meaning in this jurisdiction?',
    'Empirical analysis of outcomes: track cases where court interpretation was followed vs. overridden; track legislative attempts to override vs. acquiescence to judicial reasoning; measure compliance rates; analyze public justification patterns (does legislature cite its power or defer to court reasoning?)',
    'If courts dominate in practice: formal supremacy doctrine is false summit (mountain perspective); actual constraint is judicial influence dressed as legislative supremacy (classification should shift per jurisdiction). If legislatures dominate: doctrine matches reality (validates supremacy reading). If balanced: neither supremacy nor court dominance applies; constraint is truly hybrid (validates tangled_rope across perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(practical_power_distribution_vs_formal_doctrine, empirical, 'Empirical power distribution between courts and legislature over constitutional meaning').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(constlegislsov_tr_t0, constitutional_text__legislative_sovereignty_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(constlegislsov_tr_t10, constitutional_text__legislative_sovereignty_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(constlegislsov_tr_t20, constitutional_text__legislative_sovereignty_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(constlegislsov_be_t0, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(constlegislsov_be_t10, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(constlegislsov_be_t20, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(constlegislsov_su_t0, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(constlegislsov_su_t10, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(constlegislsov_su_t20, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__popular_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, notwithstanding_clause_invocation_mechanism).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_amendment_rigidity).

% DUAL FORMULATION NOTE:
% The legislative sovereignty reading is one of three structurally distinct constraints derived from the kernel 'constitutional_text'. Each reading locates final authority in a different branch or body (legislature, court, people). The three readings have different ε values, different beneficiary/victim structures, and different extractiveness characteristics. They are linked by network.affects_constraints to signal their common kernel and mutual structural influence: a shift from legislative to judicial supremacy changes who benefits and who bears extraction costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
