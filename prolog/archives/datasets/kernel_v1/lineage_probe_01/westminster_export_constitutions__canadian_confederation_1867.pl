% ============================================================================
% CONSTRAINT STORY: westminster_export_constitutions__canadian_confederation_1867
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westminster_canadian_1867, []).

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
 *   constraint_id: westminster_export_constitutions__canadian_confederation_1867
 *   human_readable: Westminster Export to Canadian Confederation (1867): Written Conventions in a Federal Frame
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The British North America Act, 1867 (later Constitution Act, 1867)
 *   codified Westminster parliamentary conventions into a written federal
 *   constitution — an unprecedented move that created a hybrid model:
 *   'similar in principle to that of the United Kingdom' but frozen into
 *   statutory form and layered onto a federal structure dividing powers
 *   between central and provincial governments. This reading of the
 *   Westminster export kernel represents Canada's distinctive constitutional
 *   strategy: neither pure unwritten convention (the UK model) nor pure
 *   codified rationalism (the US or French model), but an attempt to marry
 *   both. The constraint embeds coordination (federal federation required
 *   written enumeration to solve the commons problem of allocation) with
 *   extraction (imperial disallowance, Privy Council appeals, and suppression
 *   of pre-existing indigenous governance systems). The codification of
 *   conventions was itself contested: purists on both sides — those committed
 *   to unwritten constitutional flexibility and those demanding pure codified
 *   design — found the hybrid unsatisfactory. The constraint's extractiveness
 *   declined over 150 years as Canada's courts displaced the Privy Council
 *   and imperial disallowance atrophied, but the suppression of indigenous
 *   governance systems and the structural suppression of pure-model
 *   constitutionalism remain embedded in the frame.
 *
 * KEY AGENTS:
 *   - Imperial Crown: Primary beneficiary (institutional/arbitrage) — retained disallowance and appellate power; codification locked in imperial authority
 *   - Canadian Founding Elite: Secondary beneficiary (powerful/constrained) — solved the federation coordination problem; consolidated elite control through constitutional entrenchment
 *   - Hybrid Constitutionalism Advocates: Organized beneficiaries (organized/mobile) — genuinely solved a coordination problem; represented by Confederation movement
 *   - Pure Unwritten Tradition Purists: Primary victim (powerless/trapped) — Westminster conventions now rigid; unwritten model rendered illegible
 *   - Pure Codified Model Purists: Primary victim (powerless/trapped) — hybrid satisfies neither rationalist constitutionalism; incoherent constitutional form
 *   - Provincial Sovereignty Maximalists: Secondary victims/constrained agents (moderate/constrained) — constrained by federal compact but protected by enumerated powers
 *   - Indigenous Governance Systems: Suppressed victim (powerful precontact/trapped postcontact) — pre-existing governance erased and subordinated under Westminster export
 *   - Canadian Judiciary (post-1940): Emerging beneficiary (organized/mobile) — gradual displacement of Privy Council increased Canadian constitutional autonomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westminster_export_constitutions__canadian_confederation_1867, 0.52).
domain_priors:suppression_score(westminster_export_constitutions__canadian_confederation_1867, 0.48).
domain_priors:theater_ratio(westminster_export_constitutions__canadian_confederation_1867, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westminster_export_constitutions__canadian_confederation_1867, extractiveness, 0.52).
narrative_ontology:constraint_metric(westminster_export_constitutions__canadian_confederation_1867, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(westminster_export_constitutions__canadian_confederation_1867, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westminster_export_constitutions__canadian_confederation_1867, tangled_rope).
narrative_ontology:human_readable(westminster_export_constitutions__canadian_confederation_1867, "Westminster Export to Canadian Confederation (1867): Written Conventions in a Federal Frame").
narrative_ontology:topic_domain(westminster_export_constitutions__canadian_confederation_1867, "political/legal/constitutional").

domain_priors:requires_active_enforcement(westminster_export_constitutions__canadian_confederation_1867).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westminster_export_constitutions__canadian_confederation_1867, '68534b69-19d8-4201-a7a0-702593f30abb').
narrative_ontology:cs_kernel_codification('68534b69-19d8-4201-a7a0-702593f30abb', formalized).
narrative_ontology:cs_authority_grounding('68534b69-19d8-4201-a7a0-702593f30abb', extraction).
narrative_ontology:cs_interpretation_layer_present('68534b69-19d8-4201-a7a0-702593f30abb').
narrative_ontology:cs_reading_relation('68534b69-19d8-4201-a7a0-702593f30abb', westminster_export_constitutions__australian_federation_1901, coexists_with).
narrative_ontology:cs_reading_relation('68534b69-19d8-4201-a7a0-702593f30abb', westminster_export_constitutions__decolonization_constitutions, influences).
narrative_ontology:cs_reading_relation('68534b69-19d8-4201-a7a0-702593f30abb', westminster_export_constitutions__irish_free_state_1922, coexists_with).
narrative_ontology:cs_axiom('68534b69-19d8-4201-a7a0-702593f30abb', foundational, codified_conventions_preserve_flexibility).
narrative_ontology:cs_axiom_status(codified_conventions_preserve_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('68534b69-19d8-4201-a7a0-702593f30abb', codified_conventions_preserve_flexibility, empirically_contingent).
narrative_ontology:cs_axiom('68534b69-19d8-4201-a7a0-702593f30abb', foundational, hybrid_model_solves_federation_commons).
narrative_ontology:cs_axiom_status(hybrid_model_solves_federation_commons, holdable).
narrative_ontology:cs_axiom_grounding('68534b69-19d8-4201-a7a0-702593f30abb', hybrid_model_solves_federation_commons, instrumental).
narrative_ontology:cs_reference_frame('68534b69-19d8-4201-a7a0-702593f30abb', hybrid_westminster_constitutionalism).
narrative_ontology:cs_drift_state('68534b69-19d8-4201-a7a0-702593f30abb', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('68534b69-19d8-4201-a7a0-702593f30abb', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(westminster_export_constitutions__canadian_confederation_1867, westminster_export_constitutions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westminster_export_constitutions__canadian_confederation_1867, imperial_crown).
narrative_ontology:constraint_beneficiary(westminster_export_constitutions__canadian_confederation_1867, hybrid_constitutionalism_advocates).
narrative_ontology:constraint_beneficiary(westminster_export_constitutions__canadian_confederation_1867, canadian_founding_elite).
narrative_ontology:constraint_victim(westminster_export_constitutions__canadian_confederation_1867, pure_unwritten_tradition_purists).
narrative_ontology:constraint_victim(westminster_export_constitutions__canadian_confederation_1867, pure_codified_model_purists).
narrative_ontology:constraint_victim(westminster_export_constitutions__canadian_confederation_1867, provincial_sovereignty_maximalists).
narrative_ontology:constraint_victim(westminster_export_constitutions__canadian_confederation_1867, indigenous_governance_excluded).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNWRITTEN CONSTITUTIONALISM PURIST (SNARE) — Cannot escape the codification; Westminster convention was meant to remain fluid and adaptive, evolving through practice. The Canadian Constitution Act freezes conventions into written law, rendering the traditional unwritten model illegible. Trapped in a frame that contradicts the foundation of their constitutional theory. Maximum extraction — no alternative representation possible within their philosophical framework.
constraint_indexing:constraint_classification(westminster_export_constitutions__canadian_confederation_1867, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CODIFIED CONSTITUTIONALISM PURIST (SNARE) — Equally trapped but from the opposite direction. A pure codified constitution should emerge from rationalist design and natural rights theory (as in the US or France), not from importing parliamentary practice alongside written text. The hybrid form satisfies neither model's requirements — it appears incoherent as pure constitution. Trapped in disagreement with the foundational choice.
constraint_indexing:constraint_classification(westminster_export_constitutions__canadian_confederation_1867, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: PROVINCIAL SOVEREIGNTY MAXIMALIST (TANGLED ROPE) — Constrained by the federal compact but also benefits from the constitutional text's enumerated powers framework, which protects provincial jurisdiction against centralization. The constraint both binds (provinces cannot be unilaterally dissolved) and enables (constitutional limitations on federal overreach). Moderate extraction — high cost of exit but genuine coordination benefit.
constraint_indexing:constraint_classification(westminster_export_constitutions__canadian_confederation_1867, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: IMPERIAL CROWN (ROPE) — Benefits from the written frame that locks in disallowance power and appeals to the Judicial Committee of the Privy Council. Westminster conventions become codified assets of empire, not soft constraints. Experiences the constraint as pure coordination: the written constitution communicates imperial authority. Net beneficiary — extraction runs toward empire. Low effective chi due to arbitrage exit options and beneficiary status.
constraint_indexing:constraint_classification(westminster_export_constitutions__canadian_confederation_1867, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HYBRID CONSTITUTIONALISM ADVOCATES (ROPE) — See the solution as genuine coordination: the written enumeration of powers solves federal clarity while Westminster conventions preserve parliamentary flexibility. The constraint enables something neither pure model could: federal coordination with responsible government. Organized agents (the 1867 framers, Confederation movements) perceive this as solving a collective action problem. Mobile exit — if the hybrid failed, codified constitutionalism or unwritten convention could be restored.
constraint_indexing:constraint_classification(westminster_export_constitutions__canadian_confederation_1867, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: INDIGENOUS GOVERNANCE SYSTEMS (TANGLED ROPE) — Extracted from and suppressed by the Westminster export: pre-existing Haudenosaunee, Anishinaabe, Coast Salish and other governance systems were subordinated under the imposed frame. The constraint provides NO coordination benefit for indigenous governance, only suppression of alternatives. High extraction, genuine suppression (legal prohibition on recognition). Constrained by colonial law rather than trapped — exit possible through constitutional amendment, but at substantial political cost.
constraint_indexing:constraint_classification(westminster_export_constitutions__canadian_confederation_1867, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the 1867 reading represents a genuine structural innovation: hybrid codification solved the federation coordination problem and preserved Westminster flexibility simultaneously. This was neither pure extraction (the coordination benefit is real) nor pure coordination (imperial disallowance was retained). The constraint's extractiveness reflects both coordination (federal stability) and extraction (imperial control). This perspective sees the constraint as structurally coherent, not as a false summit.
constraint_indexing:constraint_classification(westminster_export_constitutions__canadian_confederation_1867, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westminster_export_constitutions__canadian_confederation_1867_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(westminster_export_constitutions__canadian_confederation_1867, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(westminster_export_constitutions__canadian_confederation_1867, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(westminster_export_constitutions__canadian_confederation_1867, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westminster_export_constitutions__canadian_confederation_1867_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, declining over time. At founding (t=0), extractiveness was higher (~0.58) because imperial disallowance and Privy Council appeals provided external enforcement of the constitutional order. As Canadian courts displaced JCPC (completed ~1949) and disallowance atrophied, the constraint's extractiveness declined toward genuine coordination (0.48-0.50 by mid-20th century, now ~0.52). The current value reflects that genuine coordination benefits (federal stability, enumerated powers preventing commons tragedy) persist alongside extraction (constitutional rigidity suppressing unwritten flexibility, imperial residues, indigenous suppression). Suppression (0.48): Moderate, declining over time. Suppression was highest at founding (0.58) when disallowance was active and alternative constitutional frames were legally prohibited. As disallowance became unused and as indigenous governance began partial constitutional recognition (1982 onward), suppression declined. Current suppression reflects that pure-model constitutionalism remains intellectually suppressed (not recognized as viable alternative) and indigenous governance remains structurally subordinated. Theater ratio (0.38): Low-moderate, declining. The 1867 Constitution's performative content was lower than typical constraints because the writers genuinely believed written enumeration of powers was solving a real problem. Theater increased slightly when disallowance became unused (ritual without function), then declined again as courts treated the Constitution as genuinely operative. Current low theater reflects that the Constitution actually governs; it is not primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extraordinarily wide because the constraint resolves fundamentally different problems for different agents. For the imperial crown, it solves the problem of retaining authority over a grown dominion through written codification that locks in disallowance. For the founding elite, it solves the federation commons problem (provinces need boundaries, central government needs enumeration). For Westminster purists, the constraint does NOT solve anything — it breaks Westminster's defining feature (unwritten flexibility). For codification purists, it solves nothing — it violates rationalist constitutional theory by mixing categories. For indigenous governance, it solves nothing and extracts everything (prior systems erased). The perspectives do not disagree about facts; they disagree about which problem the constraint was actually solving. This is a structural feature of how constitutional design works: a single choice can genuinely coordinate for some parties while genuinely extracting from others, because they were trying to solve different problems.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality for each perspective derives from the agent's structural relationship: (1) The imperial crown is beneficiary + arbitrage → low d (~0.10) → negative f(d) → negative χ. They experience the constraint as advantageous. (2) The pure-unwritten purist is victim + trapped → high d (~0.95) → high f(d) ~1.42 → high χ. They experience maximum extraction. (3) The provincial maximalist is victim+constrained but also benefits from enumerated protection → d ~0.55 → f(d) ~0.75 → moderate χ. (4) The hybrid advocate is beneficiary + mobile → low d (~0.15) → low/negative f(d) → low χ. They see solution to collective action problem. The engine derives these d values from the beneficiary/victim declarations and exit options; the sigmoid f(d) produces the experienced extractiveness variance across perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this reading is resolved by recognizing that 'the Westminster export to Canada' is not a single constraint but a presheaf of distinct problems: (1) the empire's problem (retaining authority) — solved by Snare (extraction via codification), (2) the federation's problem (coordinating provinces) — solved by Rope (genuine coordination via enumerated powers), (3) the tradition's problem (preserving Westminster flexibility) — solved by Snare (extraction via rigidification), (4) the design theorist's problem (coherent constitution) — solved by Snare (extraction via hybrid incoherence). The Tangled Rope classification at base level reflects that genuine coordination (federalism) coexists with genuine extraction (imperialism) in the same structural object. The constraint is not mislabeled — it is Tangled Rope because it genuinely has both a coordination function (solving federal commons) and an extraction mechanism (locking in imperial control and suppressing alternatives).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    written_convention_stability,
    'Does codifying Westminster conventions into written law preserve their adaptive function or ossify them into rigid rules?',
    'Historical evolution analysis: comparison of how written constitutional provisions (ss. 91-92 enumerated powers) adapted vs. unwritten conventions (responsible government, cabinet formation) evolved. Document cases where written text forced inflexibility vs. where conventions enabled adaptation.',
    'If codification preserves adaptability: the hybrid solves real coordination problem (Rope from more perspectives). If codification ossifies: the written form becomes dead letter while conventions govern anyway (Piton from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(written_convention_stability, empirical, 'Whether codified conventions retain adaptive function or become rigid').

omega_variable(
    imperial_extraction_retention,
    'What proportion of the 1867 Constitution''s enforceability derived from retained imperial powers (disallowance, appeals to Privy Council) versus from internal Canadian constitutional legitimacy?',
    'Archival analysis of 1867-1930 disputes: frequency and outcome of disallowance decisions, Privy Council appeals patterns, transition points where Canadian courts replaced JCPC authority. Identify the moment Canadian constitutional legitimacy became independent of imperial backup.',
    'If imperial powers were essential to early compliance: extractiveness should be higher (~0.60+) because the constraint''s force came from external coercion. If internal legitimacy developed quickly: extractiveness reflects genuine hybrid solution with embedded extraction (~0.50-0.55, current classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imperial_extraction_retention, empirical, 'Proportion of 1867 enforceability from imperial vs. internal legitimacy').

omega_variable(
    sibling_reading_alternative_trajectories,
    'How would Canada''s constitutional development differ if it had adopted the Australian Washminster model (federal + elected senate) or purely unwritten Westminster drift, instead of written hybrid?',
    'Counterfactual institutional analysis: model Australian-style second chamber effects on federalism; compare UK''s unwritten constitutional flexibility with Canada''s written rigidity in practice. Document cases where alternative models would have produced different outcomes.',
    'If Washminster would have solved coordination better: the Canadian reading is suboptimal and extractiveness reflects institutional path-dependence rather than genuine coordination (~0.58, Snare from analytical view). If unwritten drift would have worked: the reading is an unnecessary constraint (extractiveness ~0.65, Snare). If hybrid remains optimal: current Tangled Rope classification stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_alternative_trajectories, conceptual, 'Counterfactual institutional effectiveness of alternative Westminster exports').

omega_variable(
    kernel_contest_framings,
    'Are the four sibling readings (Australian, Canadian, decolonization, Irish) genuinely distinct constitutional strategies, or are they instances of a single underlying extraction mechanism (metropolitan control via template)?',
    'Structured comparison of extractiveness values across all four readings. If they cluster around similar ε (0.48-0.58), the kernel is ''metropolitan export mechanism.'' If they diverge significantly, each reading represents a distinct strategy with its own coordination-extraction profile.',
    'If clustered: the kernel contest is over whether hybrid solutions legitimate or mask extraction. If divergent: each reading has its own structural logic and the contest is over which model was imported and how it was adapted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_framings, conceptual, 'Whether sibling readings represent distinct strategies or instances of extraction').

omega_variable(
    responsible_government_convention_enforcement,
    'How was the unwritten convention of responsible government actually enforced in early Canadian federation when it was not explicitly written into the Constitution Act?',
    'Constitutional history of Governor General behavior 1867-1920: document instances where Westminster convention would have required action not mandated by written text. Identify mechanisms of convention transmission (example-setting, constitutional custom, informal political pressure).',
    'If conventions were enforced through example and custom: the written-plus-unwritten hybrid genuinely solved coordination (Rope classification valid). If conventions were repeatedly violated or required explicit later codification: the 1867 reading relied on unstable assumptions (extractiveness higher, ~0.58).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(responsible_government_convention_enforcement, empirical, 'Actual enforcement mechanisms for unwritten Westminster conventions in Canada').

omega_variable(
    false_summit_natural_law_risk,
    'Is the 1867 hybrid constitutional model presented as a natural solution to federation (and therefore as inevitable and unchangeable), or is it recognized as a contingent choice that other paths were available?',
    'Historiography analysis: examine constitutional commentary and political discourse 1867-present. Identify moments where alternatives (US federalism, pure Westminster drift, pure codification) were explicitly rejected vs. moments where they were presented as impossible or unthinkable.',
    'If hybrid is naturalized: risk of false summit classification — the reading could be reframed as Mountain (inevitable). If alternatives remain visible: the reading remains clearly Tangled Rope (contingent coordination choice with embedded extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, conceptual, 'Whether the 1867 model is naturalized or recognized as contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westminster_export_constitutions__canadian_confederation_1867, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(westm_can_tr_t0, westminster_export_constitutions__canadian_confederation_1867, theater_ratio, 0, 0.45).
narrative_ontology:measurement(westm_can_tr_t30, westminster_export_constitutions__canadian_confederation_1867, theater_ratio, 30, 0.38).
narrative_ontology:measurement(westm_can_tr_t60, westminster_export_constitutions__canadian_confederation_1867, theater_ratio, 60, 0.36).

% Extraction over time
narrative_ontology:measurement(westm_can_be_t0, westminster_export_constitutions__canadian_confederation_1867, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(westm_can_be_t20, westminster_export_constitutions__canadian_confederation_1867, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(westm_can_be_t40, westminster_export_constitutions__canadian_confederation_1867, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(westm_can_be_t60, westminster_export_constitutions__canadian_confederation_1867, base_extractiveness, 60, 0.48).
narrative_ontology:measurement(westm_can_be_t80, westminster_export_constitutions__canadian_confederation_1867, base_extractiveness, 80, 0.52).
narrative_ontology:measurement(westm_can_be_t100, westminster_export_constitutions__canadian_confederation_1867, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(westm_can_su_t0, westminster_export_constitutions__canadian_confederation_1867, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(westm_can_su_t40, westminster_export_constitutions__canadian_confederation_1867, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(westm_can_su_t100, westminster_export_constitutions__canadian_confederation_1867, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westminster_export_constitutions__canadian_confederation_1867, enforcement_mechanism).
narrative_ontology:affects_constraint(westminster_export_constitutions__canadian_confederation_1867, westminster_export_constitutions__australian_federation_1901).
narrative_ontology:affects_constraint(westminster_export_constitutions__canadian_confederation_1867, westminster_export_constitutions__decolonization_constitutions).
narrative_ontology:affects_constraint(westminster_export_constitutions__canadian_confederation_1867, westminster_export_constitutions__irish_free_state_1922).
narrative_ontology:affects_constraint(westminster_export_constitutions__canadian_confederation_1867, canadian_disallowance_power).
narrative_ontology:affects_constraint(westminster_export_constitutions__canadian_confederation_1867, privy_council_appellate_jurisdiction).

% DUAL FORMULATION NOTE:
% The Canadian 1867 reading is linked to three sibling readings of the same kernel and to two downstream constraints (disallowance and Privy Council appeals) that were component mechanisms embedded in the hybrid frame. The three siblings represent alternative Westminster export strategies; the two downstream constraints capture the extraction mechanisms that made the 1867 codification effective as an instrument of imperial control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westminster_export_constitutions__canadian_confederation_1867, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
