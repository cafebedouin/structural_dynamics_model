% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__pragmatic_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__pragmatic_accommodation_reading, []).

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
 *   constraint_id: kami_buddha_ontology__pragmatic_accommodation_reading
 *   human_readable: Kami-Buddha Coexistence as Pragmatic Accommodation (No Coherent Ontology Reading)
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   From the 9th century (emergence of honji suijaku doctrine) through the
 *   Edo period, Japanese religious institutions maintained a pragmatic
 *   coexistence of kami veneration and buddhism without coherent ontological
 *   framework. This reading claims that the apparent syncretism masks an
 *   incoherent bundle of institutional arrangements: particular monasteries
 *   cooperated with particular shrines; specific kami were paired with
 *   specific buddhas; but no systematic logic unified these pairings. The
 *   honji suijaku doctrine — claiming kami are traces (suijaku) of buddha
 *   originals (honji) — operated as post-hoc rationalization of political
 *   convenience, not as discovery of metaphysical truth. The arrangement
 *   extracted significant benefits for the imperial court (maintained
 *   leverage over both institutions), established monasteries (gained access
 *   to shrine lands and pilgrim revenues), and patronized priesthoods
 *   (received court grants through dual-tradition legitimacy). The
 *   constraint's primary victims were lay practitioners (trapped in
 *   contradictory devotional demands) and religious coherence itself
 *   (subjected to performative rather than genuine unification). Theater
 *   ratio rises over the interval (0.45 → 0.80) as honji suijaku doctrine
 *   matures and becomes more elaborate at disguising incoherence.
 *   Extractiveness and suppression_requirement also rise as institutional
 *   enforcement intensifies to maintain the ambiguity against reform
 *   pressures.
 *
 * KEY AGENTS:
 *   - Lay Practitioners: Primary victims (powerless/trapped) — bear cost of contradictory ritual demands with no ontological resolution
 *   - Established Monasteries: Primary beneficiaries (institutional/arbitrage) — gain shrine access, pilgrim revenues, political legitimacy through dual-tradition claims
 *   - Court-Patronized Priesthoods: Primary beneficiaries (institutional/arbitrage) — receive court grants and legitimacy through coexistence arrangement
 *   - Imperial Court: Primary beneficiary and power maintainer (powerful/constrained) — extracts leverage over both institutions by maintaining incoherence
 *   - Honji Suijaku Doctrine: Performative rationalization (institutional/arbitrage) — post-hoc explanation that masks rather than resolves contradiction
 *   - Reform Movements / Meiji Modernizers: Organized pressure (organized/constrained) — push for coherence and systematization; increase suppression requirement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__pragmatic_accommodation_reading, 0.58).
domain_priors:suppression_score(kami_buddha_ontology__pragmatic_accommodation_reading, 0.62).
domain_priors:theater_ratio(kami_buddha_ontology__pragmatic_accommodation_reading, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__pragmatic_accommodation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(kami_buddha_ontology__pragmatic_accommodation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(kami_buddha_ontology__pragmatic_accommodation_reading, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__pragmatic_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__pragmatic_accommodation_reading, "Kami-Buddha Coexistence as Pragmatic Accommodation (No Coherent Ontology Reading)").
narrative_ontology:topic_domain(kami_buddha_ontology__pragmatic_accommodation_reading, "religious_studies/comparative_religion/japanese_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__pragmatic_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__pragmatic_accommodation_reading, 'effba3d9-6407-4b7f-b15a-d2d4b7800002').
narrative_ontology:cs_kernel_codification('effba3d9-6407-4b7f-b15a-d2d4b7800002', distributed).
narrative_ontology:cs_authority_grounding('effba3d9-6407-4b7f-b15a-d2d4b7800002', extraction).
narrative_ontology:cs_reading_relation('effba3d9-6407-4b7f-b15a-d2d4b7800002', kami_buddha_ontology__unified_manifestation_reading, forecloses).
narrative_ontology:cs_reading_relation('effba3d9-6407-4b7f-b15a-d2d4b7800002', kami_buddha_ontology__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('effba3d9-6407-4b7f-b15a-d2d4b7800002', foundational, no_coherent_ontology_exists).
narrative_ontology:cs_axiom_status(no_coherent_ontology_exists, holdable).
narrative_ontology:cs_axiom_grounding('effba3d9-6407-4b7f-b15a-d2d4b7800002', no_coherent_ontology_exists, empirically_contingent).
narrative_ontology:cs_axiom('effba3d9-6407-4b7f-b15a-d2d4b7800002', foundational, honji_suijaku_post_hoc_rationalization).
narrative_ontology:cs_axiom_status(honji_suijaku_post_hoc_rationalization, holdable).
narrative_ontology:cs_axiom_grounding('effba3d9-6407-4b7f-b15a-d2d4b7800002', honji_suijaku_post_hoc_rationalization, empirically_contingent).
narrative_ontology:cs_reference_frame('effba3d9-6407-4b7f-b15a-d2d4b7800002', pragmatic_institutional_arrangement).
narrative_ontology:cs_drift_state('effba3d9-6407-4b7f-b15a-d2d4b7800002', meiji_separation_edict, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('effba3d9-6407-4b7f-b15a-d2d4b7800002', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__pragmatic_accommodation_reading, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__pragmatic_accommodation_reading, imperial_court).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__pragmatic_accommodation_reading, established_buddhist_institutions).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__pragmatic_accommodation_reading, shinto_priesthoods).
narrative_ontology:constraint_victim(kami_buddha_ontology__pragmatic_accommodation_reading, religious_coherence).
narrative_ontology:constraint_victim(kami_buddha_ontology__pragmatic_accommodation_reading, lay_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAY PRACTITIONER (SNARE) — Trapped within irreconcilable devotional demands. Must perform kami veneration, buddha worship, and shinto purification without coherent ontological framework. Suppression is structural: local ritual masters enforce competing requirements; exit means social exclusion. No alternative spiritual path is available. Maximum experienced extraction — caught in contradictory demands with no escape.
constraint_indexing:constraint_classification(kami_buddha_ontology__pragmatic_accommodation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ESTABLISHED BUDDHIST MONASTERY (ROPE) — Benefits from the pragmatic accommodation: gains access to kami shrine lands, pilgrim revenues, and political legitimacy through both buddhist and shinto patronage. Experiences the non-coheherence as manageable complexity, not constraint. Can arbitrage between systems — claiming both buddha doctrine AND kami affinity. Net beneficiary; coordination function is real (managing dual-tradition rituals).
constraint_indexing:constraint_classification(kami_buddha_ontology__pragmatic_accommodation_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: COURT-PATRONIZED SHINTO PRIESTHOOD (ROPE) — Receives imperial patronage and land grants through coexistence arrangement. Legitimacy depends on maintaining the ambiguity — a fully coherent unified system would eliminate their independent role. Arbitrage position: can claim both kami authenticity AND buddha sophistication. Benefits from the pragmatic mess.
constraint_indexing:constraint_classification(kami_buddha_ontology__pragmatic_accommodation_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: IMPERIAL COURT (TANGLED ROPE) — Primary beneficiary (powerful/constrained). Uses the pragmatic accommodation to maintain leverage over both religious institutions: can threaten increased buddhist patronage to rein in shinto priesthoods, and vice versa. Extracts legitimacy from both systems simultaneously. Constrained because dismantling the ambiguity would require choosing sides, losing control. Genuine coordination function: manages inter-institutional balance through ambiguity. Substantial extraction: keeps both institutions dependent on imperial validation.
constraint_indexing:constraint_classification(kami_buddha_ontology__pragmatic_accommodation_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HONJI SUIJAKU THEORETICAL DOCTRINE (PITON) — Post-hoc rationalization framework claiming kami are traces (suijaku) of buddha originals (honji). Operates as performative theory: makes the incoherence appear coherent to external observers and to elites. Theater ratio ≥ 0.80: the doctrine is 80%+ theater — it does not resolve the actual ontological contradiction, merely masks it with elegant language. Function has atrophied: doctrine persists through institutional inertia, not because it genuinely reconciles anything. Classified as piton because it is substantially performative.
constraint_indexing:constraint_classification(kami_buddha_ontology__pragmatic_accommodation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NECESSITY VIEW (MOUNTAIN) — Treats the pragmatic accommodation as inevitable: given two religious traditions with institutional power, some coexistence mechanism is a natural law of colonial/contact-zone dynamics. The incoherence is just what stable plural religious coexistence looks like. However, this reading's own structure contradicts the mountain classification: the extracted beneficiaries (court, monasteries, priesthoods) and suppressed victims (lay practitioners, religious coherence) reveal that the arrangement is contingent institutional power, not natural law. Engine's false summit detector will flag this.
constraint_indexing:constraint_classification(kami_buddha_ontology__pragmatic_accommodation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: REFORM MOVEMENTS / MEIJI MODERNIZERS (TANGLED ROPE) — Organized agents (intellectual currents, reformers) who reject the incoherent accommodation and push for systematization (separation of kami and buddha, 'return' to pure shinto or pure buddhism). Experience the pragmatic mess as constraint on modernization. Their pressure increases suppression_requirement in later periods as they demand coherence. Chi moderate: constrained by institutional resistance but also benefit from reformist institutional support.
constraint_indexing:constraint_classification(kami_buddha_ontology__pragmatic_accommodation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__pragmatic_accommodation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kami_buddha_ontology__pragmatic_accommodation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kami_buddha_ontology__pragmatic_accommodation_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__pragmatic_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(kami_buddha_ontology__pragmatic_accommodation_reading, TR),
    TR >= 0.70.

:- end_tests(kami_buddha_ontology__pragmatic_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts primarily through maintaining institutional power asymmetries: court controls both institutions through ambiguity; monasteries monopolize shrine cooperation; priesthoods depend on dual-legitimacy. Lay practitioners have no alternative spiritual systems — extraction from them is total but diffuse (cultural/devotional cost rather than material). The measurement trajectory (0.35 → 0.58) reflects increasing institutional entrenchment. Suppression (0.62): Moderate-high and structural. Barriers to exit include local ritual enforcement (violating kami worship brings community penalties), economic dependence on temple-shrine infrastructure, and absence of alternative coherent systems. Suppression is not violent coercion but structural: the only way to participate in Japanese religious life is through acceptance of the incoherent framework. Theater ratio (0.80): High and rising. Honji suijaku doctrine is 80%+ performative by the 1200s: it does not reconcile the ontological contradiction (kami are not logically equivalent to buddha manifestations under standard buddhist metaphysics). The doctrine claims equivalence through rhetoric, not resolution. The measurement rise (0.45 → 0.80) reflects that as institutional stakes increase, the performative framing becomes more elaborate — more sophisticated rationalization masks deeper incoherence. Claimed type is tangled_rope because: (1) genuine coordination function exists (managing dual-tradition rituals, balancing institutional powers, enabling pilgrim flows through shrine-temple complexes), (2) asymmetric extraction occurs (court benefits more than lay practitioners; monasteries benefit more than priesthoods), (3) requires_active_enforcement is true (institutional actors must continuously maintain the ambiguity against coherence-seeking pressure).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits perspectival divergence across five distinct agent positions. Lay practitioners see snare: trapped in contradictory demands, no exit. Monasteries and priesthoods see rope: coordination mechanism that benefits them. Court sees tangled rope: mixed coordination (balancing two institutions) and extraction (maintaining leverage). The honji suijaku doctrine itself operates as piton: performative maintenance of incoherence through increasingly elaborate rationalization. Reform movements see tangled rope: constraint on modernization that must be overcome. The analytical observer at civilizational scope risks seeing mountain (treating pragmatic accommodation as natural law of plural coexistence), but the structural data (identified beneficiaries and victims, active enforcement, rising theater) reveals this as false summit — the arrangement is contingent institutional power, not necessity. The deepest gap: between the court's perspective (incoherence as strategic advantage) and lay practitioners' perspective (incoherence as trap). No single institutional actor genuinely wants coherence; all benefit from ambiguity. Only lay practitioners and the abstract good of religious coherence lose.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's beneficiary/victim status and exit options. The imperial court: beneficiary + constrained (cannot exit without losing control) → d ≈ 0.45. Monasteries: beneficiaries + arbitrage (can shift allegiance) → d ≈ 0.15. Priesthoods: beneficiaries + arbitrage → d ≈ 0.15. Lay practitioners: victims + trapped (no exit) → d ≈ 0.95. Religious coherence as abstract victim: powerless + trapped → d ≈ 1.0. The high d values for trapped agents drive high f(d), producing the snare classification from their perspective. The low d values for arbitrage-enabled beneficiaries drive low f(d), producing rope from their perspectives. The court's constrained exit (cannot abandon either institution without strategic loss) produces moderate d and tangled_rope classification.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_coherence_measurement,
    'How do we measure whether an ontology is genuinely coherent or merely performs coherence rhetorically?',
    'Systematic analysis of honji suijaku texts: do they attempt to resolve logical contradiction, or do they assert equivalence without resolving? Do practitioners report subjective coherence or functional pragmatism?',
    'If honji suijaku genuinely resolves: reclassify as unified_manifestation_reading; tangled_rope may become rope. If purely rhetorical: pragmatic_accommodation reading confirmed as accurate; piton status of doctrine confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_coherence_measurement, conceptual, 'Whether honji suijaku framework is genuinely coherent or performatively coherent').

omega_variable(
    alternative_ontology_counterfactual,
    'Would a fully separated ontology (domain partition) or a fully unified ontology (honji suijaku formalized as doctrine of record) have been politically sustainable given imperial power structures?',
    'Comparative analysis of domains where separation WAS achieved (Meiji Shinto Separation Edict) and domains where unification WAS formalized (Pure Land buddhism); examination of institutional conflict and stability under each model.',
    'If separation or unification would have been sustainable: pragmatic accommodation reading becomes contingent strategy choice, not necessity. If both would have destabilized court control: pragmatic accommodation becomes an optimized extraction structure, increasing chi for court perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_ontology_counterfactual, empirical, 'Political sustainability of alternative ontological arrangements').

omega_variable(
    lay_practitioner_subjective_coherence,
    'Do historical lay practitioners report experiencing kami-buddha coexistence as coherent or as contradictory/confusing?',
    'Analysis of lay texts, letters, confessional records, and folk interpretations of honji suijaku doctrine; comparison with post-Meiji separation lay narratives about religious clarity.',
    'If practitioners report coherence: suppression and snare classification are weakened; constraint becomes rope from lay perspective. If practitioners report confusion: snare classification confirmed; suppression is structural fact, not analytical imposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_practitioner_subjective_coherence, empirical, 'Lay subjective experience of kami-buddha incoherence').

omega_variable(
    institutional_enforcement_of_ambiguity,
    'Did institutional actors (monasteries, priesthoods, court) actively enforce the incoherent accommodation, or did incoherence emerge through passive institutional accumulation?',
    'Historical analysis of edicts, letters, and institutional policy: did temples explicitly forbid coherent systematization? Did priesthoods actively suppress competing ontological frameworks? Or did ambiguity persist through lack of enforcement of any single system?',
    'If active enforcement: requires_active_enforcement is correct; tangled_rope classification is robust. If passive accumulation: constraint is more piton-like (inertial, not actively maintained); may reclassify to piton from more perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_enforcement_of_ambiguity, empirical, 'Whether institutional actors actively enforced incoherent accommodation').

omega_variable(
    reading_boundary_ambiguity,
    'Is the boundary between the pragmatic_accommodation reading and the unified_manifestation reading a fundamental disagreement about what happened, or a difference in how the same institutional reality is interpreted?',
    'Analysis of honji suijaku doctrine: is it a posteriori rationalization (pragmatic reading) or a priori metaphysical commitment (unified reading)? Compare temporal sequencing: did institutional accommodation precede or follow the doctrine? Did doctrine participants claim to be discovering truth or inventing explanations?',
    'If rationalization: pragmatic reading is correct; unified reading is false downstream interpretation. If metaphysical commitment: unified reading''s axiom (kami_and_buddha_manifestations_of_ultimate) was the framework institutions operated from; pragmatic reading mischaracterizes the actors'' own understanding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Whether pragmatic accommodation preceded and motivated doctrine or doctrine grounded accommodation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__pragmatic_accommodation_reading, 800, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kb_prag_tr_t800, kami_buddha_ontology__pragmatic_accommodation_reading, theater_ratio, 800, 0.45).
narrative_ontology:measurement(kb_prag_tr_t1000, kami_buddha_ontology__pragmatic_accommodation_reading, theater_ratio, 1000, 0.65).
narrative_ontology:measurement(kb_prag_tr_t1200, kami_buddha_ontology__pragmatic_accommodation_reading, theater_ratio, 1200, 0.8).

% Extraction over time
narrative_ontology:measurement(kb_prag_be_t800, kami_buddha_ontology__pragmatic_accommodation_reading, base_extractiveness, 800, 0.35).
narrative_ontology:measurement(kb_prag_be_t1000, kami_buddha_ontology__pragmatic_accommodation_reading, base_extractiveness, 1000, 0.48).
narrative_ontology:measurement(kb_prag_be_t1200, kami_buddha_ontology__pragmatic_accommodation_reading, base_extractiveness, 1200, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(kb_prag_su_t800, kami_buddha_ontology__pragmatic_accommodation_reading, suppression_requirement, 800, 0.4).
narrative_ontology:measurement(kb_prag_su_t1000, kami_buddha_ontology__pragmatic_accommodation_reading, suppression_requirement, 1000, 0.55).
narrative_ontology:measurement(kb_prag_su_t1200, kami_buddha_ontology__pragmatic_accommodation_reading, suppression_requirement, 1200, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__pragmatic_accommodation_reading, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__pragmatic_accommodation_reading, kami_buddha_ontology__unified_manifestation_reading).
narrative_ontology:affects_constraint(kami_buddha_ontology__pragmatic_accommodation_reading, kami_buddha_ontology__domain_partition_reading).
narrative_ontology:affects_constraint(kami_buddha_ontology__pragmatic_accommodation_reading, meiji_shinto_separation_mandate).

% DUAL FORMULATION NOTE:
% The kami_buddha_ontology kernel decomposes into three distinct constraint stories representing three competing readings of 1200 years of Japanese religious coexistence. Each reading has its own ε, its own beneficiary/victim structure, and its own classification signature. The pragmatic_accommodation_reading has ε=0.58 (moderate-high extraction masked by performative rationalization). The unified_manifestation_reading would have lower ε (genuine doctrinal coherence reduces extraction cost for lay practitioners). The domain_partition_reading would have lower suppression (functional separation eliminates contradictory devotional demands). These are not measurable differences of the same constraint but structurally distinct claims about what the 1200-year coexistence actually was. The Meiji Shinto Separation Edict (late 1800s) provides empirical pressure: it forcibly implemented domain partition, validating or falsifying claims about whether the prior pragmatic arrangement was sustainable. Network affects_constraints links this story to its sibling readings and to the historical moment that tested them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__pragmatic_accommodation_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
