% ============================================================================
% CONSTRAINT STORY: popular_assemblies_and_tribunate__comitia_centuriata_timocracy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_comitia_centuriata_timocracy, []).

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
 *   constraint_id: popular_assemblies_and_tribunate__comitia_centuriata_timocracy
 *   human_readable: Centuriate Assembly: Popular Sovereignty Organized by Wealth Class
 *   domain: legal/doctrinal/constitutional
 *
 * SUMMARY:
 *   The centuriate assembly (comitia centuriata) of the Roman Republic
 *   embodied a structural tension: it was formally the assembly of the people
 *   (populus Romanus), yet its voting procedure systematized wealth-based
 *   suppression of the unpropertied. Citizens were organized into 193
 *   centuries (centuriae) nominally representing military units but actually
 *   organized by census property classes. The propertied classes (the
 *   assidui, those with property above the minimum threshold) were
 *   concentrated in far fewer centuries but voted first and usually decided
 *   everything before the lower property classes could vote. The proletarii
 *   (those without census property, and therefore without formal voting
 *   weight in the centuriate assembly) constituted a large portion of the
 *   population but were essentially excluded. This constraint exhibits the
 *   diagnostic signature of a snare: the assembly's form grants the illusion
 *   of popular sovereignty while its structure ensures that property-weighted
 *   voting dominates outcomes. The performance of democratic procedure
 *   legitimates the extraction of political power by the propertied classes.
 *   The extractiveness value (0.58) reflects that the constraint is not
 *   absolute — the mechanism did permit aggregation of preferences and
 *   produced binding law, rather than naked force — but extraction is
 *   substantial because the unpropertied lack genuine alternative pathways to
 *   political power within the centuriate assembly's structure. The
 *   suppression value (0.72) reflects active enforcement: the census was
 *   administered by magistrates, voting procedures were controlled, and the
 *   weight assigned to each property class was formally specified. This is a
 *   designed constraint, not an emergent one.
 *
 * KEY AGENTS:
 *   - Proletarii (Unpropertied Citizens): Primary victims (powerless/trapped) — excluded from meaningful electoral weight; must watch the propertied decide
 *   - Plebeian Commons (Lower-Propertied Classes): Secondary victims (moderate/constrained) — have formal voting rights but vote after the propertied classes and rarely change outcomes
 *   - Propertied Centuries (Patrician and Equestrian Elite): Primary beneficiaries (institutional/arbitrage) — vote first, exercise effective decision power, experience the assembly as coordination of their own preference
 *   - Magistrate Class (Consuls, Praetors, Censors): Co-beneficiaries (institutional/arbitrage) — presiders and agenda-setters; the assembly formalizes their authority
 *   - Tribunate and Plebeian Organization: Organized opposition (organized/constrained) — develops alternative pathways (plebiscites, veto, contio) to bypass centuriate supremacy
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the centuriate assembly as a structural necessity rather than an elite-designed extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, 0.58).
domain_priors:suppression_score(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, 0.72).
domain_priors:theater_ratio(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, extractiveness, 0.58).
narrative_ontology:constraint_metric(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, snare).
narrative_ontology:human_readable(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, "Centuriate Assembly: Popular Sovereignty Organized by Wealth Class").
narrative_ontology:topic_domain(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, "legal/doctrinal/constitutional").

domain_priors:requires_active_enforcement(popular_assemblies_and_tribunate__comitia_centuriata_timocracy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, '0701216b-449e-4413-984d-069bf1aec8a8').
narrative_ontology:cs_kernel_codification('0701216b-449e-4413-984d-069bf1aec8a8', formalized).
narrative_ontology:cs_authority_grounding('0701216b-449e-4413-984d-069bf1aec8a8', extraction).
narrative_ontology:cs_interpretation_layer_present('0701216b-449e-4413-984d-069bf1aec8a8').
narrative_ontology:cs_reading_relation('0701216b-449e-4413-984d-069bf1aec8a8', popular_assemblies_and_tribunate__contio_persuasion_arena, coexists_with).
narrative_ontology:cs_reading_relation('0701216b-449e-4413-984d-069bf1aec8a8', popular_assemblies_and_tribunate__plebiscite_force_of_law, influences).
narrative_ontology:cs_reading_relation('0701216b-449e-4413-984d-069bf1aec8a8', popular_assemblies_and_tribunate__tribunician_sacrosanctity, coexists_with).
narrative_ontology:cs_axiom('0701216b-449e-4413-984d-069bf1aec8a8', foundational, centuriate_assembly_final_authority).
narrative_ontology:cs_axiom_status(centuriate_assembly_final_authority, holdable).
narrative_ontology:cs_axiom_grounding('0701216b-449e-4413-984d-069bf1aec8a8', centuriate_assembly_final_authority, conventional).
narrative_ontology:cs_axiom('0701216b-449e-4413-984d-069bf1aec8a8', foundational, property_weighted_voting_legitimacy).
narrative_ontology:cs_axiom_status(property_weighted_voting_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0701216b-449e-4413-984d-069bf1aec8a8', property_weighted_voting_legitimacy, instrumental).
narrative_ontology:cs_axiom('0701216b-449e-4413-984d-069bf1aec8a8', secondary, proletarii_exclusion_procedural_necessity).
narrative_ontology:cs_axiom_status(proletarii_exclusion_procedural_necessity, overridden).
narrative_ontology:cs_axiom_grounding('0701216b-449e-4413-984d-069bf1aec8a8', proletarii_exclusion_procedural_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('0701216b-449e-4413-984d-069bf1aec8a8', republican_assembly_sovereignty).
narrative_ontology:cs_drift_state('0701216b-449e-4413-984d-069bf1aec8a8', late_republic_second_century, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0701216b-449e-4413-984d-069bf1aec8a8', '').
narrative_ontology:cs_kernel_id(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, popular_assemblies_and_tribunate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, propertied_centuries).
narrative_ontology:constraint_beneficiary(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, patrician_magistrates).
narrative_ontology:constraint_victim(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, proletarii_unpropertied).
narrative_ontology:constraint_victim(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, plebeian_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROLETARII (SNARE) — Vote last or not at all. Electoral weight indexed to census property; below the threshold, no formal weight in the assembly. Trapped within citizenship formalism that grants vote but denies substance. Maximum extraction: the assembly's existence legitimates 'the people's' rule while the people's actual power is suppressed by the census weighting. No exit from this structural position without acquiring property, which requires wealth the system itself concentrates upward.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PLEBEIAN COMMONS (SNARE) — Those with property but below the wealth threshold for early voting centuries. Vote constrained by the weighted system; their votes are counted but typically after the propertied classes have already decided. High suppression of voice; constrained exit (could in principle accumulate property, but barriers are steep). Significant extraction: political power is nominally universal but practically concentrated.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: PROPERTIED CENTURIES (ROPE) — Coordinated through formal electoral structure. Vote first; decision usually rendered by the time lower classes vote. Experience the assembly as pure coordination: the mechanism solves the problem of aggregating elite preference and channeling it into binding law. Minimal suppression experienced by beneficiaries (their alternatives are not suppressed — they always have institutional access). Net beneficiary — the constraint subsidizes their political power.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: MAGISTRATE CLASS (ROPE) — Agenda-setters and presiders. The centuriate assembly is the institutional mechanism through which their will is ratified. They experience it as coordination of elite preference into law. Low extraction cost; high institutional benefit. The assembly formalizes their executive power through 'the people's' sovereignty — coordination mechanism that legitimates their authority.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 5: TRIBUNATE AND PLEBEIAN ORGANIZATION (TANGLED ROPE) — From a longer timescale, the constraint exhibits genuine coordination function (the assembly does aggregate votes and produce binding law) alongside asymmetric extraction (the propertied centuries' dominance). The tribunate and plebeian assembly (comitia tributa) represent an organized response: alternative coordination pathways (plebiscites, tribunician veto) that bypass the centuriate assembly's weighted structure. This perspective sees both the coordination problem (need for binding decision procedure) and the extraction (wealth-based suppression). The constraint's active enforcement (census administration, voting procedures, magistrate presiding) is necessary to maintain both functions simultaneously.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the centuriate assembly might appear as a natural law of political development: any large polity needs an aggregation mechanism; property-weighted voting reflects the 'natural' distribution of stakes and capacity to participate; the form of one-person-one-vote is impossible for large populations. This perspective sees the constraint as emerging naturally from structural requirements. However, the structural data contradicts the mountain classification: the propertied centuries explicitly vote first; the census is actively administered and enforced; suppression of the unpropertied is a designed feature, not an inescapable consequence. The engine will identify this as a false summit — naturalizing what is actually a constructed extraction mechanism.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(popular_assemblies_and_tribunate__comitia_centuriata_timocracy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(popular_assemblies_and_tribunate__comitia_centuriata_timocracy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The centuriate assembly extracts political power from the unpropertied (the proletarii get no effective vote) and from the lower-propertied classes (they vote but after decisions are made). However, extractiveness is not maximal because the assembly does produce binding law through an aggregation mechanism, and it does coordinate elite preference — there is a genuine coordination function layered with extraction. The value of 0.58 reflects that the constraint is substantially extractive while admitting a coordination component. Over the interval (0-240 years), extractiveness drifts upward (0.48 → 0.58) as wealth concentration increases in the late Republic, and the property threshold becomes more effective at suppressing lower classes. Suppression (0.72): High. Active enforcement mechanisms include: the census (administered by magistrates to assign property classes), the voting procedures (defining which centuries vote when), and implicit legal barriers (the unpropertied cannot participate in the centuriate assembly's main decision procedure). Suppression is not total (the unpropertied are not physically prevented from voting in the assembly; they are formally permitted to vote, just after decisions are made) but is substantial (the alternatives to centuriate voting — tribal assembly, contio, tribunician action — are separate pathways, not substitutes within the centuriate structure). Over the interval, suppression rises slightly (0.65 → 0.72) as wealth inequality increases and the property threshold becomes a more effective barrier. Theater Ratio (0.65): Moderate-high. The assembly's procedure contains genuine deliberation and voting, so it is not pure theater. However, the outcome is typically predetermined once the propertied centuries vote (since their centuries outnumber the lower centuries and vote first). The performative element is the assembly's legitimating narrative: it appears to embody 'the people's' will while actually enabling the propertied classes' dominance. Theater increases over the interval (0.55 → 0.65) as late-Republican wealth inequality makes the centuriate assembly's claim to popular representation increasingly strained, requiring more elaborate narrative defense.
 *
 * PERSPECTIVAL GAP:
 *   The centuriate assembly generates a strong perspectival gap. From the propertied centuries' perspective, it is pure coordination (Rope): the mechanism solves the problem of aggregating elite preference and channeling it into law. They experience the assembly as fair aggregation of citizens' votes, weighted by property because property represents stakes in the res publica. From the proletarii's perspective, it is pure extraction (Snare): the assembly legitimates the propertied classes' dominance through the form of popular voting while denying the unpropertied any real voice. The same structural fact — voting organized by property classes with the propertied voting first — appears as 'rational weighting of stakes' to the beneficiaries and 'suppression of the many' to the victims. The analytical observer from a civilizational timescale risks seeing this as a natural law (Mountain) — large polities need aggregation mechanisms, property-weighted voting reflects realistic distribution of capacity and interest — but the structural data reveals it as a false summit: the census is actively maintained, the voting order is specified in law, the threshold is chosen by magistrates. This is not natural; it is enforced. The tribunal's challenge (the sibling reading on tribunician_sacrosanctity) and the plebeian assembly's emergence (the sibling reading on plebiscite_force_of_law) represent organized responses: alternative authority structures that claim to represent the people more genuinely than the propertied centuries do.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value d (the agent's structural position relative to extraction) is derived from the agent's power level, exit options, and beneficiary/victim status. Proletarii (powerless/trapped/victims) experience maximum d (~0.95) — they are extracted from with no exit. Plebeian commons (moderate/constrained/victims) experience high d (~0.75) — they are victims but have some structural mobility (could in principle acquire property). Propertied centuries (institutional/arbitrage/beneficiaries) experience low d (~0.15) — they benefit from the constraint with multiple alternative pathways (exit via political retirement or institutional arbitrage). Magistrates (institutional/arbitrage/co-beneficiaries) experience negative d (~0.05) — they are subsidized by the assembly's structure. The tangled_rope perspective from organized plebeian actors (organized/constrained/mixed) experiences moderate d (~0.65) — they are partially victimized but also have agency and alternative pathways (plebiscite, tribunician action). The analytical perspective (analytical/analytical/observer) experiences observer d (~0.72). These directionality values feed the sigmoid f(d) function to produce effective extractiveness chi for each perspective. Beneficiaries with arbitrage options see rope (low chi); trapped agents see snare (high chi); organized opposition sees tangled_rope (moderate chi with both coordination and extraction).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    census_threshold_contingency,
    'Is the census property threshold a natural structural necessity or a contingent policy choice by the propertied classes?',
    'Comparative analysis: did other Hellenistic and Roman polities use different thresholds or purely property-weighted systems? What would happen to the assembly''s function if the threshold were lowered by half? By ninety percent?',
    'If contingent: the centuriate assembly is a designed extraction mechanism (Snare classification upheld). If necessary: portions of the constraint approach mountain-like status (structural unavoidability). Likely: threshold is partly contingent, partly embedded in functional requirements — constraint is Tangled Rope at the institutional level, Snare from victim perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(census_threshold_contingency, empirical, 'Whether census thresholds reflect structural necessity or elite design choice').

omega_variable(
    voting_decision_precedence,
    'How often do the propertied centuries'' votes actually predetermine the final outcome? Does decision typically come before lower classes vote, or do their votes occasionally matter?',
    'Historical record analysis: frequency of outcomes where majority of propertied centuries voted one way and full assembly voted differently. Examine election records, senatorial decrees, legal sources on specific votes.',
    'If propertied centuries decide first >95% of the time: Snare classification is conservative; constraint approaches pure_extraction. If they decide first 60-70%: constraint is genuine Tangled Rope (coordination happens, extraction is high but not absolute). If <50%: beneficiary analysis was wrong; constraint might be Rope or different type entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voting_decision_precedence, empirical, 'Historical frequency of propertied-century decision precedence').

omega_variable(
    contio_versus_centuriate_authority,
    'Does the contio (persuasion assembly before any formal vote) represent genuine alternative deliberative power, or is it purely preparatory theater for the centuriate assembly''s predetermined outcome?',
    'Textual analysis of sources on contio dynamics; comparison of outcomes when magistrates faced strong contio opposition versus weak. Did magistrate proposals change based on contio response?',
    'If contio is genuine alternative: the centuriate assembly''s snare classification may underestimate the system''s deliberative capacity (adds Rope component). If contio is theater: confirms Snare (formal deliberation masks predetermined outcome). The sibling reading contio_persuasion_arena disputes whether centuriate voting or contio persuasion is the real locus of power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contio_versus_centuriate_authority, empirical, 'Whether contio represents genuine alternative deliberative power or theater').

omega_variable(
    plebeian_assembly_competition,
    'After the Hortensian law, did the plebeian assembly (comitia tributa) become a genuinely competing decision mechanism, or did the centuriate assembly retain effective supremacy?',
    'Longitudinal analysis of major legislation: which assembly passed binding laws on what topics? When magistrates had choice, which did they use? Did substantive outcomes shift when plebeian assembly passed plebiscites?',
    'If plebeian assembly became competing: the centuriate assembly''s extraction power was partially curtailed by the institutional development sibling reading plebiscite_force_of_law) describes. Snare classification remains but with temporal boundary (pre-Hortensian versus post-Hortensian extractiveness). If centuriate retained supremacy: Snare classification is robust across the late-Republican period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plebeian_assembly_competition, empirical, 'Whether plebeian assembly became genuinely competing or centuriate supremacy persisted').

omega_variable(
    property_mobility_and_census_dynamics,
    'How mobile was property ownership in the Roman population? Could unpropertied citizens realistically move into propertied centuries through wealth acquisition, making exit ''mobile'' rather than ''trapped''?',
    'Economic historians'' estimates of property mobility rates; case studies of clients or freedmen who moved from unpropertied to propertied status; analysis of census updates and their frequency.',
    'If mobility is significant (>15% intergenerational movement to propertied status): some victims experience ''constrained'' or ''mobile'' exit rather than ''trapped'' — shifts some perspectives from Snare to Tangled Rope. If mobility is low (<5%): ''trapped'' classification is correct; Snare remains robust from victim perspective. Likely: mobility exists but is highly constrained by patronage, land availability, and wealth concentration — makes exit ''constrained'' or ''identity_locked'' (trapped within client relationships that prevent independent wealth accumulation) rather than purely ''trapped'' or truly ''mobile''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_mobility_and_census_dynamics, empirical, 'Property mobility and exit capacity for unpropertied citizens').

omega_variable(
    kernel_contest_boundary,
    'Which of the four sibling readings (contio_persuasion_arena, plebiscite_force_of_law, tribunician_sacrosanctity) represent genuinely competing kernels versus alternative pathways that coexist with centuriate timocracy?',
    'Doctrinal analysis: did Roman legal tradition treat these as competing authority sources or as complementary mechanisms? Which sources privilege which reading? Did late-Republican practice treat contio, plebiscite, or tribunician veto as overriding centuriate assembly authority?',
    'Determines reading_relations: if truly competing kernels, relations should include ''forecloses'' for readings that directly contradict (e.g., if tribunician sacrosanctity reading claims tribune''s veto supersedes assembly, it might foreclose centuriate timocracy''s claim to final decision authority). If coexisting mechanisms, relations are ''coexists_with'' or ''influences''. Likely: all four coexist as distinct institutional pathways with different authority bases and constituencies; the ''real'' constitution was the presheaf over all four readings, not any single one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_boundary, conceptual, 'Status of kernel readings as competing versus coexisting mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_early_republic, popular_assemblies_and_tribunate__comitia_centuriata_timocracy, theater_ratio, 0, 0.55).
narrative_ontology:measurement(theater_middle_republic, popular_assemblies_and_tribunate__comitia_centuriata_timocracy, theater_ratio, 120, 0.62).
narrative_ontology:measurement(theater_late_republic, popular_assemblies_and_tribunate__comitia_centuriata_timocracy, theater_ratio, 240, 0.65).

% Extraction over time
narrative_ontology:measurement(extractiveness_early_republic_midfifth_century, popular_assemblies_and_tribunate__comitia_centuriata_timocracy, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(extractiveness_middle_republic_fourth_century, popular_assemblies_and_tribunate__comitia_centuriata_timocracy, base_extractiveness, 120, 0.55).
narrative_ontology:measurement(extractiveness_late_republic_second_century, popular_assemblies_and_tribunate__comitia_centuriata_timocracy, base_extractiveness, 240, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(suppression_early_republic, popular_assemblies_and_tribunate__comitia_centuriata_timocracy, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(suppression_middle_republic, popular_assemblies_and_tribunate__comitia_centuriata_timocracy, suppression_requirement, 120, 0.7).
narrative_ontology:measurement(suppression_late_republic, popular_assemblies_and_tribunate__comitia_centuriata_timocracy, suppression_requirement, 240, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, resource_allocation).
narrative_ontology:affects_constraint(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, contio_persuasion_arena).
narrative_ontology:affects_constraint(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, plebiscite_force_of_law).
narrative_ontology:affects_constraint(popular_assemblies_and_tribunate__comitia_centuriata_timocracy, tribunician_sacrosanctity).

% DUAL FORMULATION NOTE:
% The centuriate assembly is one of four readings of the kernel 'popular_assemblies_and_tribunate'. This reading emphasizes the assembly's wealth-weighted voting structure and the dominance of the propertied centuries. The sibling reading contio_persuasion_arena disputes whether the assembly's vote was ever truly determinative or was merely ratification of contio debates. The sibling reading plebiscite_force_of_law claims that plebeian plebiscites created a competing assembly after 287 BCE, dividing authority. The sibling reading tribunician_sacrosanctity claims the tribunate's veto was the ultimate constraint on assembly authority. All four coexist as distinct institutional pathways; the 'real' constitution was likely the presheaf over all four, not any single one. This story isolates the centuriate assembly's extractiveness and suppression from the other pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
