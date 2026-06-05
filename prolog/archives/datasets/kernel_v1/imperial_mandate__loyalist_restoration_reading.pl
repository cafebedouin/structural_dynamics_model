% ============================================================================
% CONSTRAINT STORY: imperial_mandate__loyalist_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__loyalist_restoration_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: imperial_mandate__loyalist_restoration_reading
 *   human_readable: Imperial Mandate: Loyalist Restoration Reading (Direct Sovereignty)
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   The loyalist restoration reading of the imperial mandate kernel asserts
 *   that legitimate sovereignty is unmediated: the emperor must personally
 *   exercise both ritual and administrative authority, and intermediary
 *   governance structures (shogunate, samurai bureaucracy, daimyo regional
 *   authority) are by definition usurpations of imperial prerogative. This
 *   reading emerged as a political force in 19th-century East Asia,
 *   particularly in Japan, where loyalist intellectuals and lower-ranking
 *   samurai invoked direct imperial authority to delegitimize the Tokugawa
 *   shogunate and mobilize restoration movements. The constraint generates
 *   tension across all institutional actors: it delegitimizes the entire
 *   administrative class that carries the complex governance functions of a
 *   large polity, promises direct imperial rule as the alternative, and
 *   creates severe suppression costs as intermediate structures resist
 *   demolition. The theater ratio rises over time (from 0.45 to 0.78) as
 *   restoration movements shift from intellectual critique to institutional
 *   practice: the gap widens between the reading's claims about unmediated
 *   sovereignty and the actual bureaucratic machinery required to govern,
 *   creating performative assertions of direct imperial authority that mask
 *   ongoing delegation.
 *
 * KEY AGENTS:
 *   - Imperial Court / Restoration Faction (institutional/arbitrage): Primary beneficiary — centralizes authority, eliminates competing power centers, legitimizes direct imperial initiative in foreign affairs and modernization
 *   - Loyalist Intellectuals and Low-Rank Samurai (moderate/identity_locked): Intermediary beneficiary-victims — advance through the restoration movement but are trapped by identity fusion with the legitimacy claim; exit requires abandoning foundational commitment to emperor as supreme sovereign
 *   - Samurai Bureaucratic Class (powerless/trapped): Primary victim — entire professional identity, social status, and livelihood depend on intermediate governance structures the reading delegitimizes; no alternative employment or status pathway available
 *   - Bakufu Administrative Apparatus (organized/constrained): Institutional victim-target — must dismantle itself while managing administrative continuity; constrained by organizational inertia and the need to maintain basic governance functions during transition
 *   - Analytical Observer (analytical/analytical): Can adopt natural law perspective claiming unmediated sovereignty is the inherent order, or empirical perspective recognizing it as contested institutional reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, 0.58).
domain_priors:suppression_score(imperial_mandate__loyalist_restoration_reading, 0.72).
domain_priors:theater_ratio(imperial_mandate__loyalist_restoration_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Imperial Mandate: Loyalist Restoration Reading (Direct Sovereignty)").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, 'd2f5e626-3463-4106-9e96-7fa39b1fcc9b').
narrative_ontology:cs_kernel_codification('d2f5e626-3463-4106-9e96-7fa39b1fcc9b', formalized).
narrative_ontology:cs_authority_grounding('d2f5e626-3463-4106-9e96-7fa39b1fcc9b', lineage).
narrative_ontology:cs_interpretation_layer_present('d2f5e626-3463-4106-9e96-7fa39b1fcc9b').
narrative_ontology:cs_reading_relation('d2f5e626-3463-4106-9e96-7fa39b1fcc9b', imperial_mandate__bakufu_delegation_reading, coexists_with).
narrative_ontology:cs_axiom('d2f5e626-3463-4106-9e96-7fa39b1fcc9b', foundational, unmediated_exercise_defines_legitimacy).
narrative_ontology:cs_axiom_status(unmediated_exercise_defines_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('d2f5e626-3463-4106-9e96-7fa39b1fcc9b', unmediated_exercise_defines_legitimacy, deontological).
narrative_ontology:cs_axiom('d2f5e626-3463-4106-9e96-7fa39b1fcc9b', foundational, intermediate_structures_are_usurpation).
narrative_ontology:cs_axiom_status(intermediate_structures_are_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('d2f5e626-3463-4106-9e96-7fa39b1fcc9b', intermediate_structures_are_usurpation, deontological).
narrative_ontology:cs_reference_frame('d2f5e626-3463-4106-9e96-7fa39b1fcc9b', unmediated_imperial_sovereignty).
narrative_ontology:cs_drift_state('d2f5e626-3463-4106-9e96-7fa39b1fcc9b', post_bakufu_administrative_entrenchment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d2f5e626-3463-4106-9e96-7fa39b1fcc9b', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, imperial_court).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, restoration_faction).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, intermediate_governance_structures).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, samurai_bureaucratic_class).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, shogunal_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SAMURAI BUREAUCRATIC CLASS (SNARE) — The reading delegitimizes the entire basis of samurai administrative authority. Exit requires abandoning professional identity, social status, and livelihood. Trapped by economic dependency on shogunal structures. Maximum extraction: career, identity, and institutional position all at stake simultaneously. The reading offers no role for intermediate governance — only direct imperial rule.
constraint_indexing:constraint_classification(imperial_mandate__loyalist_restoration_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BAKUFU ADMINISTRATIVE APPARATUS (TANGLED ROPE) — The bakufu is delegitimized but retains administrative coordination functions. Restoration requires dismantling the shogunate while preserving bureaucratic continuity. The bakufu experiences both extraction (loss of sovereign authority) and coordination pressure (necessity to manage transition). Constrained by the reading's logic but with some residual institutional agency during transition period.
constraint_indexing:constraint_classification(imperial_mandate__loyalist_restoration_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IMPERIAL COURT / RESTORATION FACTION (ROPE) — Primary beneficiary. The reading legitimizes direct imperial governance and centralizes authority. Court experiences this as pure coordination: establishing administrative control, consolidating legitimacy, and managing foreign engagement all flow from the restoration principle. Low experienced extraction; the constraint exists to enable the court's expansion of authority.
constraint_indexing:constraint_classification(imperial_mandate__loyalist_restoration_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LOYALIST INTELLECTUALS / LOW-RANK SAMURAI (TANGLED ROPE) — Identity fused with restoration ideology; professional advancement depends on the reading's success. Exit would require abandoning foundational identity commitments (loyalty to the emperor as supreme sovereign, rejection of shogunal usurpation as illegitimate). However, these agents also coordinate the intellectual and political infrastructure of restoration. Mixed experience: extraction from their prior positions in shogunal hierarchy, coordination through their role in restoration movement.
constraint_indexing:constraint_classification(imperial_mandate__loyalist_restoration_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational lens, the reading invokes a natural law argument: imperial sovereignty is by nature indivisible and unmediated; delegation to intermediaries is by definition illegitimate usurpation; direct rule is the natural order of legitimate governance. This perspective naturalizes what is actually a contested institutional reading. The engine will flag this as a false summit candidate.
constraint_indexing:constraint_classification(imperial_mandate__loyalist_restoration_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__loyalist_restoration_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(imperial_mandate__loyalist_restoration_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(imperial_mandate__loyalist_restoration_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__loyalist_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reading extracts through delegitimization: it strips authority and legitimacy from intermediate structures without providing immediate substitutes. Beneficiaries (imperial court, restoration faction) gain authority consolidation. Victims (samurai class, bakufu apparatus) lose professional legitimacy. The extractiveness is not maximal (not 0.85+) because the reading also coordinates a coherent ideology — it is not pure extraction but extraction coupled with an alternative governance vision. Suppression (0.72): High. Eliminating samurai bureaucracy requires severe coercion — the class has institutional power, military capacity, social status, and centuries of organizational precedent. Economic dependency on shogunal structures is near-total for samurai. Exit options are virtually nonexistent: samurai cannot simply leave governance without losing livelihood, identity, and social position. The suppression reflects both structural barriers (no alternative career paths) and the reading's own demand for institutional rupture. Theater ratio (0.65, rising to 0.78): Moderate-high and increasing. Early in the restoration period, theater is lower because intellectuals are still making theoretical critiques of shogunal legitimacy. As restoration movements gain institutional power and claim to exercise unmediated imperial authority, the theater rises: emperors appoint ministers and generals (not exercising unmediated authority); they initiate foreign policy through advisors (not personally); they modernize the bureaucracy through appointed reformers (not direct imperial hand). The claims of unmediated sovereignty become increasingly performative even as the institutional change succeeds. By the peak of restoration consolidation (t=30), theater reaches 0.78: the reading has achieved its goal (dismantled the bakufu), but the emperor's actual governance still requires massive intermediate structures, creating a gap between the restoration's theoretical legitimacy claim and its operational reality.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits stark perspectival divergence. The imperial court sees the reading as pure coordination (Rope): establishing legitimate central authority, coordinating the transition, enabling decisive response to foreign engagement and modernization. Restoration intellectuals see mixed coordination-extraction (Tangled Rope): they coordinate the intellectual and political movement while extracting authority from intermediate structures, but they are identity-locked to the reading's success. The samurai bureaucratic class sees pure extraction (Snare): their entire professional world is delegitimized with no alternative offered; the reading offers them only dissolution or subordination. The bakufu apparatus sees a forced institutional transition (Tangled Rope): it must both maintain basic governance (coordination) and accept elimination of its own authority structure (extraction). The analytical observer risks naturalizing the reading as a law of sovereignty (Mountain: 'unmediated authority is the natural form of legitimate rule'), when structural data reveals it as a contested political reading with identifiable beneficiaries and victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values reflect each agent's structural position relative to the reading. The imperial court as beneficiary with arbitrage options (can pursue alternative legitimacy claims if restoration fails) derives d≈0.15, producing low experienced extraction χ. Loyalist intellectuals as moderate/identity_locked agents are harder to model: their power is constrained by dependence on restoration success, but their identity is fused with the reading itself—they have no stable 'outside' position from which to arbitrage, so d≈0.35, producing moderate χ. The samurai class as powerless/trapped derives d≈0.95, producing maximum f(d)≈1.42 and experienced extraction χ approaching the theoretical ceiling. The bakufu as organized/constrained (can resist but faces institutional collapse if it does) derives d≈0.70, producing substantial χ≈0.65. These differences in directionality explain why the same constraint produces different classifications: beneficiary sees coordination (low d → low χ), victim sees extraction (high d → high χ), intermediate actors see mixed dynamics (moderate d → moderate χ).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy in the classical sense. Both the core coordination function (centering legitimate governance in the emperor) and the asymmetric extraction (delegitimizing intermediate structures) are coherently present in the reading. The mandatrophy risk lies elsewhere: in whether unmediated imperial sovereignty is practically viable. If the reading requires genuinely unmediated authority but actual governance requires massive delegation, the reading collapses into pure theater (Piton). This is resolvable by an omega variable examining whether the reading's premise is structurally achievable. The constraint's claim is internally coherent; the ambiguity is whether its core premise can be instantiated in practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unmediated_sovereignty_necessity,
    'Is unmediated imperial exercise of sovereignty structurally possible given scale, technology, and administrative complexity of the polity?',
    'Comparative historical analysis: periods of actual unmediated imperial rule vs. periods with inevitable delegated governance; archival evidence of emperor''s actual vs. claimed administrative reach; logistical capacity constraints',
    'If unmediated governance is structurally impossible: the reading is aspirational theater (Piton), not a functional constraint. If possible: the reading may be structurally viable (Tangled Rope confirmed). The entire classification depends on whether the reading''s core premise is practically instantiable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unmediated_sovereignty_necessity, empirical, 'Whether unmediated imperial sovereignty is structurally achievable').

omega_variable(
    restoration_vs_reform_boundary,
    'Does this reading require complete institutional rupture and replacement, or can it coexist with reformed (rather than abolished) intermediate structures?',
    'Textual analysis of restoration ideology; historical outcomes of restoration movements claiming unmediated sovereignty; whether post-restoration states retained any intermediate authority structures',
    'If rupture required: the suppression and theater values are accurate (institutional destruction is costly and performative). If reform possible: suppression drops substantially and the constraint may shift toward Rope (pure coordination of authority reorganization).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_vs_reform_boundary, conceptual, 'Necessity of institutional rupture vs. reformed delegation').

omega_variable(
    foreign_engagement_sovereigntylock,
    'Does unmediated imperial engagement with foreign powers require the emperor to personally conduct diplomacy and military strategy, or can delegation to appointed representatives preserve the reading''s legitimacy claim?',
    'Analysis of restoration rhetoric on foreign engagement; examination of how post-restoration states handled international relations; whether the reading permits administrative delegation while claiming direct imperial governance',
    'If personal imperial engagement required: theater_ratio is underestimated (personal governance is operationally impossible at scale); extraction is higher. If delegation permitted: the reading is more institutionally compatible; theater ratio drops; it may collapse toward pure Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_engagement_sovereigntylock, conceptual, 'Whether sovereignty claim permits administrative delegation').

omega_variable(
    kernel_reading_decomposition,
    'Is this constraint a single unified reading of the imperial mandate kernel, or does it mask two distinct constraints: (1) ideological claim about legitimacy (unmediated = legitimate), and (2) practical claim about governance structure (unmediated = achievable)?',
    'Decomposition test: apply ε-invariance principle. If measuring the constraint via ''ideological coherence'' gives ε≈0.25 but measuring via ''practical administrative viability'' gives ε≈0.65, they are two separate constraints. Separate stories with different omegas would be more precise.',
    'If decomposed: the current story represents the ideological legitimacy claim (Mountain or Rope candidate). The practical governance claim is a downstream constraint with higher extractiveness (Snare or Tangled Rope). The bakufu_delegation_reading would then coexist with multiple constraint stories, not just one sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Whether ideological legitimacy and practical governance are one constraint or two').

omega_variable(
    intermediate_structures_elimination,
    'What happens to the coordination functions previously provided by samurai hierarchy, regional daimyo governance, and bakufu bureaucracy when the reading eliminates these structures?',
    'Institutional replacement analysis: does restoration rhetoric propose successor institutions, or does it assume coordination will spontaneously emerge from direct imperial governance? Historical examination of how restored governments handled regional administration, military command, taxation, dispute resolution.',
    'If no replacement proposed: the reading is pure extraction (Snare), and the empire faces institutional collapse. If replacement proposed: the reading coordinates transition to alternative structures (Tangled Rope). This affects the claimed_type classification fundamentally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intermediate_structures_elimination, empirical, 'Successor institutions for eliminated intermediate structures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imandlr_tr_t0, imperial_mandate__loyalist_restoration_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(imandlr_tr_t15, imperial_mandate__loyalist_restoration_reading, theater_ratio, 15, 0.65).
narrative_ontology:measurement(imandlr_tr_t30, imperial_mandate__loyalist_restoration_reading, theater_ratio, 30, 0.78).

% Extraction over time
narrative_ontology:measurement(imandlr_be_t0, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(imandlr_be_t15, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(imandlr_be_t30, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(imandlr_su_t0, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(imandlr_su_t15, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(imandlr_su_t30, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__loyalist_restoration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imperial_mandate__loyalist_restoration_reading, 0.1).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, imperial_mandate__bakufu_delegation_reading).

% DUAL FORMULATION NOTE:
% The imperial mandate kernel grounds two distinct constraints with different extractiveness profiles. The loyalist restoration reading (this story, ε≈0.58) asserts direct sovereignty and delegitimizes intermediate structures, producing high suppression and theater. The bakufu delegation reading (ε≈0.35) permits delegated authority while maintaining ultimate imperial sovereignty, producing lower suppression and theater. Both are readings of the same kernel; the sibling stories must be linked via network.affects_constraints to enable the analysis system to detect that the perspectival gap is not mere observer disagreement but structural difference in how each reading decomposes the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
