% ============================================================================
% CONSTRAINT STORY: francophone_education_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_francophone_education_systems, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: francophone_education_systems
 *   human_readable: Francophone Education Systems: Language Preservation vs. Economic Mobility
 *   domain: education/cultural_policy/economics
 *
 * SUMMARY:
 *   Francophone education systems enforce French as the medium of instruction
 *   across primary and secondary education in France, Belgium, Switzerland,
 *   Canada (parts), and numerous African nations. The constraint exhibits
 *   tension between language preservation (cultural coordination function)
 *   and linguistic/economic mobility (extraction mechanism). For
 *   francophone-native students, French-medium instruction coordinates
 *   cultural identity and institutional continuity. For non-francophone
 *   speakers and multilingual learners, monolingual enforcement creates
 *   barriers to academic access, induces suppression through limited
 *   curriculum choice, and generates extraction through economic disadvantage
 *   (French education underperforms in English-dominated global knowledge
 *   systems). The theater ratio (0.65) reflects that French educational
 *   bureaucracies maintain performative legitimacy through certification,
 *   standardized testing, and institutional frameworks while academic
 *   outcomes increasingly diverge from international benchmarks. Theater has
 *   risen over the 30-year measurement interval as globalization has exposed
 *   the economic costs of monolingual education without corresponding policy
 *   reform. The extractiveness trajectory (0.38→0.52) shows gradual
 *   accumulation of extraction as digital globalization, international higher
 *   education, and labor market dynamics reveal the constraint's true costs.
 *   This is a diagnostic case of how a legitimate coordination mechanism
 *   (language standardization) becomes progressively extractive as external
 *   conditions shift, creating a tangled rope structure that resists reform
 *   because beneficiaries control enforcement.
 *
 * KEY AGENTS:
 *   - Rural Francophone Students: Primary victims (powerless/trapped) — bear costs of monolingual enforcement with no alternative pathways
 *   - Non-Francophone Minority Students: Primary victims (powerless/trapped) — structurally excluded; forced to acquire academic French alongside content; systematic underperformance
 *   - Urban Francophone Learners: Secondary victims (moderate/constrained) — benefit from language alignment but constrained by curriculum rigidity; face extraction at higher education and in global knowledge systems
 *   - Francophone Middle Class: Mixed position (powerful/constrained) — benefit from cultural capital signaling but constrained by limited international language exposure; can purchase supplementary education
 *   - French Cultural Establishment: Primary beneficiary (institutional/arbitrage) — maintains soft power, cultural institutions, and demographic coherence through French-medium enforcement; experiences constraint as pure coordination
 *   - Educational Reform Coalition: Organized agents (organized/mobile) — multilingual educators, minority advocates, economic developers pushing for multilingual alternatives; perceive sunset pathway for monolingual enforcement
 *   - Francophone Educational Bureaucracy: Institutional inertia (institutional/arbitrage) — maintains degraded system through institutional identity and career path dependence; recognizes declining function but experiences high political cost of reform
 *   - Analytical Observer: Civilizational frame (analytical/analytical) — risks naturalizing monolingual enforcement as inherent to education systems rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(francophone_education_systems, 0.52).
domain_priors:suppression_score(francophone_education_systems, 0.58).
domain_priors:theater_ratio(francophone_education_systems, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(francophone_education_systems, extractiveness, 0.52).
narrative_ontology:constraint_metric(francophone_education_systems, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(francophone_education_systems, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(francophone_education_systems, tangled_rope).
narrative_ontology:human_readable(francophone_education_systems, "Francophone Education Systems: Language Preservation vs. Economic Mobility").
narrative_ontology:topic_domain(francophone_education_systems, "education/cultural_policy/economics").

domain_priors:requires_active_enforcement(francophone_education_systems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(francophone_education_systems, french_cultural_institutions).
narrative_ontology:constraint_beneficiary(francophone_education_systems, francophone_political_elites).
narrative_ontology:constraint_beneficiary(francophone_education_systems, french_metropolitan_institutions).
narrative_ontology:constraint_victim(francophone_education_systems, multilingual_learners).
narrative_ontology:constraint_victim(francophone_education_systems, rural_francophone_communities).
narrative_ontology:constraint_victim(francophone_education_systems, non_francophone_minorities).
narrative_ontology:constraint_victim(francophone_education_systems, lower_socioeconomic_francophone_students).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL FRANCOPHONE STUDENT (SNARE) — Structurally trapped. Limited school options, mandatory French-medium instruction regardless of linguistic background, no meaningful choice of educational pathway. Bear costs of monolingual enforcement (alienation from local multilingual reality, academic underperformance if non-francophone dominant) with no exit. Maximum suppression: material barriers (geography, cost), institutional barriers (curriculum inflexibility), and internalized barriers (cultural messaging that French-only is 'correct'). Zero degrees of freedom.
constraint_indexing:constraint_classification(francophone_education_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NON-FRANCOPHONE MINORITY STUDENT (SNARE) — Trapped in a system designed for francophone majorities. Structurally excluded from early-stage language acquisition in native language, forced to acquire academic French alongside academic content. No alternative pathway available. Educational outcomes systematically lower; suppression operates through institutional design (curriculum), material barriers (resource scarcity for minority-language programs), and legal frameworks (language requirements). Extraction: disproportionate academic burden, limited access to higher education, reduced economic mobility.
constraint_indexing:constraint_classification(francophone_education_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: URBAN FRANCOPHONE LEARNER (TANGLED ROPE) — Benefits from French-medium instruction aligned with home language but constrained by curriculum rigidity that limits exposure to other languages (English, regional languages, other mobility languages). Genuine coordination function: French education coordinates cultural continuity and builds shared francophone identity. Extraction mechanism: constrained linguistic mobility in globalized economy; English-heavy international higher education and research creates disadvantage for French-educated students. High suppression (limited curricular options, peer pressure toward monolingualism, institutional resistance to multilingual pedagogy) but some agency through supplementary education.
constraint_indexing:constraint_classification(francophone_education_systems, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FRENCH CULTURAL ESTABLISHMENT (ROPE) — Benefits substantially from French-as-medium-of-instruction policy. Experiences the constraint as pure coordination: French education systems build and maintain the francophone community, support cultural institutions (Académie française, French literary tradition), and sustain political/diplomatic soft power. Low suppression experienced (no external barriers to French language use). High arbitrage: can exit to English-medium systems, can modify enforcement, can adjust policy at will. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(francophone_education_systems, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FRANCOPHONE MIDDLE CLASS (TANGLED ROPE) — Complex position. Benefits from French-medium education aligning with home language and cultural capital (French education signals francophone elite status, enables career advancement within francophone institutions). But constrained by limited curricula in international languages and subjects; faces extraction when children reach higher education (must acquire English rapidly in universities, competitive disadvantage in STEM fields where English dominates). Active enforcement creates asymmetry: beneficiaries pay lower cost of compliance than non-francophone groups. Moderate suppression (cultural messaging that French education is 'superior,' institutional pressure to maintain monolingualism), but also some capacity for supplementary action (private English tutoring, international schools) — hence constrained rather than trapped.
constraint_indexing:constraint_classification(francophone_education_systems, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EDUCATIONAL REFORM COALITION (SCAFFOLD) — Organized actors (multilingual educators, minority-language advocates, economic development organizations) see French-monolingual enforcement as a temporary coordination problem with an exit path. Perceive multilingual pedagogy as a rising alternative that addresses both equity (non-francophone students) and economic mobility (English/international languages). Low experienced extraction because the coalition has agency and a visible sunset: bilingual/multilingual curricula, language-flexible early education, and competency-based frameworks are accumulating across francophone countries. Suppression is real (institutional resistance, teacher training gaps, political backlash) but perceived as surmountable. The constraint classifies as Scaffold because the coalitions view enforcement as declining and alternatives as viable within a decade.
constraint_indexing:constraint_classification(francophone_education_systems, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: FRANCOPHONE EDUCATIONAL BUREAUCRACY (PITON) — The institutional machinery of french-medium instruction persists through inertia rather than function. High theater ratio (0.65): standardized testing in French, curriculum frameworks, pedagogical approaches, official certifications all maintain performative legitimacy while academic outcomes diverge from international benchmarks (French STEM education underperforms in international assessments relative to multilingual systems). The bureaucracy experiences the constraint as degraded — officials recognize that French-monolingual enforcement is losing functional justification (economic integration pressures, digital globalization, student mobility) yet institutional change is slow. Theater is sustained by institutional identity (education ministry as guardian of French), career paths built on monolingual frameworks, and political risk of appearing to abandon French language protection.
constraint_indexing:constraint_classification(francophone_education_systems, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal frame, some form of language-of-instruction standardization may appear inevitable: all education systems require a common medium to coordinate curriculum and assessment. This perspective risks naturalizing French-monolingual enforcement as an immutable feature of how education systems function. However, the structural data contradicts the mountain classification — the presence of beneficiaries, victims, active enforcement, and high theater suggests this is a contingent institutional arrangement, not a law of nature. The 'natural' appearance derives from the normalization of monolingualism in 20th-century nation-state education systems, which is itself a historical contingency. The engine's false summit detector will flag this perspective as a naturalization error.
constraint_indexing:constraint_classification(francophone_education_systems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(francophone_education_systems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(francophone_education_systems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(francophone_education_systems, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(francophone_education_systems, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(francophone_education_systems, TR),
    TR >= 0.70.

:- end_tests(francophone_education_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from non-francophone speakers and multilingual learners through reduced academic access, limited international language exposure, and economic disadvantage. For francophone speakers, extraction is lower (language aligns with home use) but still present (constrained curriculum, underperformance in international knowledge systems). The value reflects that approximately 60% of students in francophone systems (non-francophone minorities and multilingual learners) experience significant extraction, while 40% (francophone natives) experience mixed coordination/extraction. Suppression (0.58): Moderate-high. Multiple layers: material barriers (limited minority-language resources, geographic constraints on educational choice), institutional barriers (curriculum inflexibility, language requirements for advancement, assessment frameworks designed for monolingual learners), legal frameworks (language-of-instruction mandates in many jurisdictions), and internalized barriers (cultural messaging that French monolingualism is 'correct' and multilingualism is 'confusion'). Theater ratio (0.65): Moderate-high. Performative components include standardized testing, curriculum certifications, and pedagogical legitimacy claims that persist despite declining outcomes in international assessments. French STEM education underperforms similar-resource systems using multilingual approaches, yet this gap does not trigger policy response — indicating that theater maintenance (protecting 'French education' as a brand) exceeds outcome optimization.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how a coordinating mechanism for one group (francophones) becomes extractive for others (non-francophones). The gap between Rope (beneficiary view) and Snare (victim view) at the same biographical time horizon reveals that the binding mechanism is institutional enforcement, not inherent natural law. If the constraint were a Mountain, all perspectives would converge on the same type. The fact that beneficiaries see Rope while victims see Snare indicates that the constraint exists because it is enforced for someone's benefit — making it extractive by definition. The reform coalition's Scaffold perspective reveals that alternatives are structurally possible, which further confirms that the current constraint is contingent, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from structural position: beneficiary/victim status + exit options + power level. French cultural establishment: beneficiary + arbitrage → low d → negative f(d) → low experienced extraction. Urban francophone learner: mixed (partial beneficiary, partial victim) + constrained → moderate d. Non-francophone student: victim + trapped → high d → high f(d) → high experienced extraction. The engine computes d from the beneficiary/victim declarations and exit options; perspectives then incorporate d into chi calculations. The spread from d≈0.05 (institutional beneficiary) to d≈0.95 (powerless victim) produces the perspectival cascade from Rope to Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in francophone education systems takes the form of whether language standardization is a legitimate coordination function (Rope) or an extractive asymmetry (Snare). The resolution: both are true simultaneously from different perspectives. The constraint is Tangled Rope because it combines genuine coordination (for francophone speakers, French-medium instruction stabilizes a shared culture and enables institutional continuity) with genuine asymmetric extraction (for non-francophone speakers, monolingual enforcement creates barriers and reduces economic mobility). The mandatrophy resolves by recognizing that the extraction is not intrinsic to language standardization itself, but to the *specific choice* of French monolingualism as the standard. Multilingual approaches would preserve coordination (shared institutional frameworks, language retention) while reducing extraction (broader access, international mobility). The constraint persists because the choice benefits a specific coalition (French cultural establishment, francophone political elites) whose power is sufficient to maintain enforcement despite the accumulating extraction costs for other groups. This is the canonical form of Tangled Rope: genuine coordination function (language standardization) plus genuine extraction (asymmetric beneficiary distribution) plus active enforcement (required because the distribution is not self-sustaining from powerless agent consent).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    language_of_instruction_threshold,
    'What proportion of students can experience monolingual instruction as a genuine coordination mechanism (aligned with home language + cultural capital) versus extraction (misalignment with home language or limited economic mobility outcomes)?',
    'Cohort-level outcome analysis: track educational attainment, language proficiency (both French and international languages), economic mobility, and subjective wellbeing by home language background and instructional model over 15-year periods',
    'If majority experience alignment: constraint is primarily Rope or Tangled Rope. If majority experience extraction: constraint is primarily Snare or Piton with degraded function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(language_of_instruction_threshold, empirical, 'Proportion of students experiencing monolingual instruction as coordination vs extraction').

omega_variable(
    multilingual_alternative_feasibility,
    'Can francophone countries maintain cultural/political cohesion while adopting multilingual (French + regional language + English/other) curricula?',
    'Comparative case analysis: track francophone countries implementing multilingual systems (e.g., parts of Canada, Switzerland, Belgium) on cultural cohesion metrics (language retention in francophone population, identity stability, institutional continuity) and economic outcomes (higher education access, international mobility, STEM competitiveness) over 20 years',
    'If feasible with positive outcomes: scaffold sunset is real and faster than currently estimated. If cultural fragmentation occurs: enforcement may be justified on cohesion grounds, reframing extraction as necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multilingual_alternative_feasibility, empirical, 'Feasibility of multilingual curricula while maintaining francophone cohesion').

omega_variable(
    english_medium_extraction_floor,
    'Is rapid switch to English-medium instruction (avoiding current extraction) merely replacing one extraction mechanism (French monolingualism) with another (English hegemony), or does it genuinely improve outcomes?',
    'Longitudinal outcome tracking in countries transitioning to English-medium education on: non-English-speaker academic performance, L1 language maintenance, access to non-English knowledge traditions, and long-term socioeconomic outcomes. Comparison with multilingual and French-medium systems.',
    'If English creates new extraction: the constraint family is ''language-of-instruction extraction systems'' with French-monolingual and English-monolingual as two manifestations. If multilingual reduces extraction: scaffold perspective is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(english_medium_extraction_floor, empirical, 'Whether English-medium instruction reduces or replaces extraction').

omega_variable(
    early_childhood_language_plasticity,
    'Do multilingual early-childhood programs (ages 3-7) impede French language development or enhance cognitive/linguistic capacity without penalizing French acquisition?',
    'Randomized trial or quasi-experimental design comparing monolingual, bilingual, and multilingual early education on French proficiency, other language proficiency, executive function, and school readiness at age 7 and school entry. Track outcomes through secondary education.',
    'If multilingual early education maintains French + builds other languages: enforcement of monolingualism from age 3 is purely extractive. If monolingual advantage persists: some extraction is coordination cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(early_childhood_language_plasticity, empirical, 'Effects of multilingual early education on French acquisition and cognitive development').

omega_variable(
    theater_versus_function_decomposition,
    'How much of the apparent educational effectiveness of French-medium instruction is real (French as medium enables learning) versus theater (cultural capital signaling, institutional legitimacy)?',
    'Outcome decomposition: compare students with equivalent French proficiency in monolingual vs multilingual systems; isolate language-medium effect from cultural capital, institutional prestige, and peer composition effects. Use instrumental variables or matching methods.',
    'If theater dominates: the piton classification is confirmed and acceleration of reform is justified. If function dominates: extraction is lower than base_properties suggest, reclassify some perspectives downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_versus_function_decomposition, empirical, 'Decomposition of theater versus genuine educational function of French-medium instruction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(francophone_education_systems, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(franco_edu_tr_t0, francophone_education_systems, theater_ratio, 0, 0.52).
narrative_ontology:measurement(franco_edu_tr_t10, francophone_education_systems, theater_ratio, 10, 0.58).
narrative_ontology:measurement(franco_edu_tr_t20, francophone_education_systems, theater_ratio, 20, 0.65).
narrative_ontology:measurement(franco_edu_tr_t30, francophone_education_systems, theater_ratio, 30, 0.6).

% Extraction over time
narrative_ontology:measurement(franco_edu_be_t0, francophone_education_systems, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(franco_edu_be_t10, francophone_education_systems, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(franco_edu_be_t20, francophone_education_systems, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(franco_edu_be_t30, francophone_education_systems, base_extractiveness, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(francophone_education_systems, identity_coordination).
narrative_ontology:affects_constraint(francophone_education_systems, international_higher_education_english_dominance).
narrative_ontology:affects_constraint(francophone_education_systems, minority_language_attrition).
narrative_ontology:affects_constraint(francophone_education_systems, francophone_stem_competitiveness).

% DUAL FORMULATION NOTE:
% Francophone education systems represent one instance of language-of-instruction enforcement. The upstream constraint is language standardization as coordination function (generic); the downstream constraints are specific manifestations (French monolingualism, English-medium alternatives, minority language attrition). English-medium enforcement in post-colonial contexts exhibits parallel structure with different beneficiaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(francophone_education_systems, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
