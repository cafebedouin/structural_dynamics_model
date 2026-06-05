% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__domain_partition_reading, []).

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
 *   constraint_id: kami_buddha_ontology__domain_partition_reading
 *   human_readable: Kami-Buddha Domain Partition: Functional Ontological Separation
 *   domain: religious_studies/japanese_religion/comparative_theology
 *
 * SUMMARY:
 *   The kami-buddha domain partition is a constraint that organizes Japanese
 *   religious practice and institutional authority into functionally separate
 *   systems: kami govern life, fertility, purity, and cyclical renewal;
 *   buddhas govern death, afterlife, merit transfer, and soteriological
 *   salvation. This domain partition operates as a tangled rope — it
 *   coordinates institutional jurisdiction (both priesthoods benefit from
 *   clear boundaries) while simultaneously extracting theological coherence
 *   from the broader system. The constraint is enforced through institutional
 *   practice, liturgical specialization, and a suppressed alternative (the
 *   honji suijaku unified-manifestation theory that dominated scholarly
 *   Japanese religion for 600 years until Meiji-era political reorganization
 *   dismantled it). This constraint story instantiates the
 *   domain_partition_reading of the contested kami_buddha_ontology kernel.
 *   The reading asserts that kami and buddhas are ontologically distinct
 *   entities serving genuinely different functional domains—this is not
 *   merely institutional accommodation but structural theological necessity.
 *   However, historical analysis reveals the partition's modern form was
 *   established during the 1868–1912 Meiji Restoration when the Japanese
 *   state deliberately separated Shinto from Buddhism to serve nationalist
 *   ideology (State Shinto, emperor-worship doctrine). The
 *   unified-manifestation theory that made theoretical sense of the
 *   coexistence was formally dismantled in the process. The current domain
 *   partition is thus a 19th-century political construction that has become
 *   institutionally naturalized and theoretically defended as if it were
 *   ancient theological discovery.
 *
 * KEY AGENTS:
 *   - Village Practitioners: Primary victims (powerless/trapped) — structurally required to participate in both systems without coherent theological framework; no exit from dual-system requirement
 *   - Shinto Priesthood (Mononobe lineage): Primary beneficiary (institutional/arbitrage) — monopolizes life-domain authority (birth, purity, fertility, cyclic renewal); experiences domain partition as coordination enabling institutional stability
 *   - Buddhist Institutional Authority: Secondary beneficiary (institutional/arbitrage) — monopolizes death-domain authority (funerary rites, merit transfer, karmic soteriology); parallel beneficiary with shinto priesthood
 *   - Scholastic Theologians: Secondary victim (moderate/constrained) — benefit from intellectual coherence of two-domain framework but constrained by institutional pressure to suppress unified-manifestation scholarship and maintain separation
 *   - Imperial Theology Establishment: Complex actor (powerful/mobile) — benefits from both kami authority claims (imperial descent) and buddha authority claims (merit-transfer legitimacy) without resolving contradiction; extracting theological flexibility from incoherence
 *   - Theological Coherence: Abstract victim — the domain partition suppresses and extracts coherence from the broader Japanese religious system by enforcing separation without unified justification
 *   - Unified-Manifestation Scholarship: Suppressed alternative — honji suijaku theory offered genuine coherence but was formally dismantled during Meiji political reconstruction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition_reading, 0.38).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition_reading, 0.42).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition_reading, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition_reading, "Kami-Buddha Domain Partition: Functional Ontological Separation").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition_reading, "religious_studies/japanese_religion/comparative_theology").

domain_priors:requires_active_enforcement(kami_buddha_ontology__domain_partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition_reading, 'd294e62c-9e71-44fd-a693-4155ab9580f0').
narrative_ontology:cs_kernel_codification('d294e62c-9e71-44fd-a693-4155ab9580f0', formalized).
narrative_ontology:cs_authority_grounding('d294e62c-9e71-44fd-a693-4155ab9580f0', extraction).
narrative_ontology:cs_interpretation_layer_present('d294e62c-9e71-44fd-a693-4155ab9580f0').
narrative_ontology:cs_reading_relation('d294e62c-9e71-44fd-a693-4155ab9580f0', kami_buddha_ontology__unified_manifestation_reading, coexists_with).
narrative_ontology:cs_reading_relation('d294e62c-9e71-44fd-a693-4155ab9580f0', kami_buddha_ontology__pragmatic_accommodation_reading, influences).
narrative_ontology:cs_axiom('d294e62c-9e71-44fd-a693-4155ab9580f0', foundational, kami_buddha_ontological_independence).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_independence, holdable).
narrative_ontology:cs_axiom_grounding('d294e62c-9e71-44fd-a693-4155ab9580f0', kami_buddha_ontological_independence, deontological).
narrative_ontology:cs_axiom('d294e62c-9e71-44fd-a693-4155ab9580f0', foundational, functional_domain_necessity).
narrative_ontology:cs_axiom_status(functional_domain_necessity, overridden).
narrative_ontology:cs_axiom_grounding('d294e62c-9e71-44fd-a693-4155ab9580f0', functional_domain_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('d294e62c-9e71-44fd-a693-4155ab9580f0', kami_buddha_functional_separation_framework).
narrative_ontology:cs_drift_state('d294e62c-9e71-44fd-a693-4155ab9580f0', contemporary_post_war_religious_studies_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d294e62c-9e71-44fd-a693-4155ab9580f0', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition_reading, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition_reading, shinto_priesthood).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition_reading, buddhist_institutional_authority).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition_reading, theological_coherence).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition_reading, lay_religious_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VILLAGE PRACTITIONER (SNARE) — Structurally trapped within dual-system requirement without coherent framework to navigate it. Must perform both kami and buddha rituals for life-cycle events (birth/naming via kami, death via buddha) but receives no unified theological guidance. The separation feels mandatory and natural, enforced by both priesthoods who claim authority over distinct domains. No exit: abandoning either system means ritual incompleteness and social exclusion.
constraint_indexing:constraint_classification(kami_buddha_ontology__domain_partition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SCHOLASTIC THEOLOGIAN (TANGLED ROPE) — Constrained by institutional pressure to maintain domain partition while genuinely benefiting from the intellectual coherence it provides. The two-domain framework enables sophisticated theological work — one can study kami-purity systems without needing to resolve them into buddha-nature metaphysics, and vice versa. Yet the theologian also bears costs: must suppress unified-manifestation scholarship (honji suijaku theory) to maintain separation, faces career risk if proposing alternative ontologies, and witnesses growing incoherence as folk practice increasingly blurs the boundaries.
constraint_indexing:constraint_classification(kami_buddha_ontology__domain_partition_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SHINTO PRIESTHOOD (ROPE) — Primary beneficiary with maximum arbitrage. The domain partition grants them exclusive authority over purity, birth, fertility, and cyclical agricultural rituals — a functional monopoly in life-domain religion. Experiences the constraint as pure coordination: the separation enables clear jurisdictional boundaries, mutual recognition with buddhist institutions, and stable institutional identity. Net benefit with minimal extraction cost.
constraint_indexing:constraint_classification(kami_buddha_ontology__domain_partition_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: BUDDHIST INSTITUTIONAL AUTHORITY (ROPE) — Parallel beneficiary with maximum arbitrage. The domain partition grants them exclusive authority over death, merit transfer, and karmic-soteriological frameworks. Experiences the constraint as coordination enabling institutional stability and jurisdictional clarity. Benefits from the separation's enforceability — both priesthoods have incentive to maintain boundaries because deviation would undermine each side's authority claim. Net benefit aligned with shinto counterpart.
constraint_indexing:constraint_classification(kami_buddha_ontology__domain_partition_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: IMPERIAL THEOLOGY ESTABLISHMENT (TANGLED_ROPE) — Powerful institutional actor with mobile exit options but structurally invested in maintaining the partition. The dual-system framework serves imperial interests: allows claiming both kami authority (Shinto state cult, imperial descent myths) and buddha authority (imperial merit-transfer rituals, bodhisattva status claims) without forcing coherence. Benefits from the incoherence because it enables multiple legitimacy claims simultaneously. Yet also constrained by the need to suppress unified-manifestation theories that would require choosing between kami and buddha primacy — a choice that would undermine either Shinto nationalist claims or Buddhist institutional reach. Extraction flow is complex: extracting theological coherence from the broader system to maintain imperial flexibility.
constraint_indexing:constraint_classification(kami_buddha_ontology__domain_partition_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: MEIJI-ERA INSTITUTIONAL APPARATUS (PITON) — From civilizational distance, the domain partition is increasingly revealed as theater. Historical scholarship shows that honji suijaku (unified-manifestation) theory was the dominant intellectual framework for 600+ years (12th–18th centuries) and was only formally dismantled during Meiji Restoration (1868–1912) when Shinto was separated from Buddhism to serve state nationalist ideology. The current domain-partition framework is a 19th-century political construction presented as ancient theological necessity. The constraint persists through institutional inertia — both priesthoods continue maintaining boundaries, scholars continue teaching the separation as structural, lay practice continues bifurcating, but the unified theory that made the partition theoretically coherent has largely atrophied in active scholarship. Theater ratio reflects: the partition is maintained performatively through institutional assertion rather than through genuine theological content.
constraint_indexing:constraint_classification(kami_buddha_ontology__domain_partition_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: NATURAL LAW VIEW / THEOLOGICAL NATURALISM (MOUNTAIN) — From a universalizing analytical perspective, the kami-buddha partition might appear as an immutable natural law: perhaps there IS a fundamental structural distinction between life-purity systems (kami) and death-soteriological systems (buddha) that is discovered rather than constructed. This perspective risks naturalizing what the historical and institutional data reveals as a politically contingent 19th-century partition. The engine's false-summit detector will identify this as a naturalization of constructed institutional arrangements rather than a genuine natural law.
constraint_indexing:constraint_classification(kami_buddha_ontology__domain_partition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__domain_partition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kami_buddha_ontology__domain_partition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kami_buddha_ontology__domain_partition_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(kami_buddha_ontology__domain_partition_reading, TR),
    TR >= 0.70.

:- end_tests(kami_buddha_ontology__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The domain partition extracts institutional monopoly control (beneficiaries get exclusive jurisdiction), enforced separation of scholarly frameworks, and suppression of alternative ontologies. However, extractiveness is not maximal because the constraint provides genuine coordination benefits: clear institutional boundaries enable stable coexistence, practitioners receive systematized ritual guidance, and theologians can develop sophisticated domain-specific scholarship. The intermediate value reflects that the tangled rope classification is accurate — genuine coordination function exists alongside asymmetric extraction. The measurement trajectory (0.22 → 0.38 over 300 years, corresponding roughly to 1650–1950) shows accumulating extraction as institutional specialization deepened and unified-manifestation scholarship was systematically suppressed during Meiji reconstruction. Suppression (0.42): Moderate-high. Institutional suppression operates through multiple mechanisms: theological marginalization (honji suijaku theory pushed to periphery), career disincentives (scholars proposing unified ontologies face institutional resistance), ritual enforcement (priesthoods actively maintain domain boundaries through practice), bureaucratic separation (distinct institutional hierarchies for kami and buddha systems), and state enforcement (Meiji period created formal legal separation). Suppression is not total because unified-manifestation theory remains available in scholarly literature and some folk practice still blurs boundaries. Theater ratio (0.58): Moderate-high. The domain partition functions partly as genuine coordination (two priesthoods do need institutional boundaries) and partly as performative separation. The theater has increased over the interval as the original justification (unified-manifestation theory) has been suppressed and replaced by assertion of structural necessity. By contemporary period, the partition is largely performative — maintained through institutional inertia and theoretical defense rather than through active theological content. The Meiji-era institutional apparatus (piton perspective) explicitly shows this dynamic: the separation is enforced performatively, with reduced theological justification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence driven by structural position and exit options. The village practitioner sees mandatory bifurcation without coherence (snare). The shinto priesthood and buddhist authority see clear jurisdictional coordination with mutual benefit (rope). The imperial establishment sees flexible multiple-authority claims (tangled rope, mobile actor benefiting from non-resolution). The scholastic theologian sees intellectual constraints imposed by institutional pressure (tangled rope, constrained victim). The civilizational analytical observer risks seeing immutable theological nature (mountain) but historical data reveals this as false summit — a 19th-century political construction naturalized as ancient necessity. The piton perspective explicitly documents the theatrical maintenance of a theoretically incoherent system through institutional assertion.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Shinto priesthood (institutional/arbitrage) experiences low d → negative f(d) → benefits from the constraint despite apparent extraction role. Buddhist authority (institutional/arbitrage) follows the same path. Village practitioner (powerless/trapped) experiences high d (victim + trapped exit) → high f(d) → maximum experienced extraction. Scholastic theologian (moderate/constrained) occupies intermediate position: constrained exit + victim status produce moderate d → moderate extraction experience. Imperial establishment (powerful/mobile) has low d despite victim classification because mobile exit options enable arbitrage use of the system — they can exit either domain without cost, so experienced extraction is lower despite ontological incoherence. The constraints on theological innovation create suppression that benefits institutional actors while constraining scholars and practitioners.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    honji_suijaku_revival_feasibility,
    'Is unified-manifestation (honji suijaku) theology ontologically coherent and defensible within contemporary religious studies, or does it require suppressing empirical and philosophical critiques to revive?',
    'Systematic analysis of honji suijaku texts (12th–18th century) against contemporary philosophy of religion standards; examination of whether modern critiques of the theory reflect genuine logical problems or merely represent Meiji political rejection',
    'If coherent: the unified-manifestation reading becomes viable alternative with lower extractiveness (more cooperation, less domain enforcement). If incoherent: domain partition becomes pragmatically necessary rather than politically contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honji_suijaku_revival_feasibility, empirical, 'Whether honji suijaku theology is defensible in modern philosophical frameworks').

omega_variable(
    institutional_incentive_for_partition_maintenance,
    'Do both Shinto and Buddhist priesthoods actively enforce the domain partition through deliberate institutional mechanisms, or does the separation persist primarily through bureaucratic inertia and lack of incentive to challenge it?',
    'Historical analysis of institutional interactions (joint councils, formal agreements, disciplinary actions); examination of whether deviation from domain boundaries triggers active resistance or merely passive non-cooperation',
    'If actively enforced: constraint is better modeled as snare (high suppression, active enforcement). If inertia-based: constraint is better modeled as piton (high theater, low active enforcement). This determines whether ''separation is structural necessity'' is accurate description or post-hoc rationalization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_incentive_for_partition_maintenance, empirical, 'Degree of active institutional enforcement of kami-buddha domain boundaries').

omega_variable(
    meiji_state_ideology_contingency,
    'How far does the modern kami-buddha domain partition depend on Meiji-era state ideology (State Shinto, emperor worship, Buddhist suppression)? Would the partition have persisted without the state apparatus driving it?',
    'Comparative analysis: pre-Meiji integration patterns in folk practice and scholarship vs post-Meiji bifurcation; analysis of whether de-state-sponsorship (post-1945) would naturally lead to re-integration or whether the separation has become institutionally self-sustaining',
    'If Meiji-contingent: domain partition is revealed as political construction with lower claim to ontological necessity; extractiveness of suppression may be higher (political coercion, not just institutional coordination). If self-sustaining: partition has developed genuine structural logic that justifies continued separation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_state_ideology_contingency, empirical, 'Degree of dependence of kami-buddha partition on Meiji state ideology').

omega_variable(
    theological_coherence_reading_contest,
    'This constraint instantiates the domain_partition_reading of the kami_buddha_ontology kernel. The sibling unified_manifestation_reading claims kami and buddhas are manifestations of single ultimate reality. Which reading''s theoretical commitments would a coherent Japanese religious philosophy actually require?',
    'Comparative theology: systematic evaluation of whether domain partition or unified manifestation better explains observed religious phenomena (folk practice patterns, institutional stability, theological production, textual interpretation traditions, ethical commitments)',
    'If unified-manifestation is theoretically superior: domain_partition_reading becomes optional political choice rather than necessary theological claim; extractiveness of suppression increases (institutions actively enforce inferior model). If domain-partition is superior: reading gains ontological authority beyond institutional contingency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_coherence_reading_contest, conceptual, 'Which kernel reading reflects genuine theological necessity vs political contingency').

omega_variable(
    kernel_reading_foreclosure_possibility,
    'Do the domain_partition and unified_manifestation readings logically foreclose each other, or can they coexist as live positions within different institutional frameworks?',
    'Logical analysis of the two readings'' core axioms (see cs_structure.axioms): if ''kami retain independent ontological existence'' and ''kami are manifestations of buddha-nature'' are logically contradictory at framework-level, then forecloses relation applies. If contradiction is only apparent (resolvable through semantic reframing or different levels of analysis), then coexists_with is more accurate.',
    'Forecloses relation: one reading must be formally rejected, increasing political stakes and suppression. Coexists_with relation: both readings remain defensible, reducing enforced theoretical coherence cost and enabling pragma-dialectical pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_possibility, conceptual, 'Whether domain_partition and unified_manifestation readings logically foreclose each other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kbodpr_theater_0, kami_buddha_ontology__domain_partition_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(kbodpr_theater_150, kami_buddha_ontology__domain_partition_reading, theater_ratio, 150, 0.45).
narrative_ontology:measurement(kbodpr_theater_300, kami_buddha_ontology__domain_partition_reading, theater_ratio, 300, 0.58).

% Extraction over time
narrative_ontology:measurement(kbodpr_extract_0, kami_buddha_ontology__domain_partition_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(kbodpr_extract_150, kami_buddha_ontology__domain_partition_reading, base_extractiveness, 150, 0.3).
narrative_ontology:measurement(kbodpr_extract_300, kami_buddha_ontology__domain_partition_reading, base_extractiveness, 300, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(kbodpr_suppression_0, kami_buddha_ontology__domain_partition_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(kbodpr_suppression_150, kami_buddha_ontology__domain_partition_reading, suppression_requirement, 150, 0.39).
narrative_ontology:measurement(kbodpr_suppression_300, kami_buddha_ontology__domain_partition_reading, suppression_requirement, 300, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__domain_partition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition_reading, kami_buddha_ontology__unified_manifestation_reading).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition_reading, kami_buddha_ontology__pragmatic_accommodation_reading).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition_reading, meiji_state_shinto_separation).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition_reading, honji_suijaku_suppression).

% DUAL FORMULATION NOTE:
% The kami_buddha_ontology kernel has three constraint stories, one for each reading: domain_partition_reading (this file), unified_manifestation_reading, pragmatic_accommodation_reading. All three stories describe the same historical phenomenon but instantiate different structural ontologies and carry different extractiveness values. The epsilon values differ because the readings propose different mechanisms for how the constraint functions. Domain_partition_reading (ε=0.38) assumes functional separation is necessary; unified_manifestation_reading would have lower epsilon (separation is imposed despite coherence being available); pragmatic_accommodation_reading would have higher epsilon (incoherence is structural condition, not coordinated necessity). All three stories are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__domain_partition_reading, institutional, 0.32).
constraint_indexing:directionality_override(kami_buddha_ontology__domain_partition_reading, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
