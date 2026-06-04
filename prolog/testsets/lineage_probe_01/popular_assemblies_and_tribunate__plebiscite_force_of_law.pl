% ============================================================================
% CONSTRAINT STORY: popular_assemblies_and_tribunate__plebiscite_force_of_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_popular_assemblies_and_tribunate__plebiscite_force_of_law, []).

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
 *   constraint_id: popular_assemblies_and_tribunate__plebiscite_force_of_law
 *   human_readable: Hortensian Law: Plebiscite Force of Law (Plebeian Lawmaking Without Patrician Confirmation)
 *   domain: legal/political/constitutional
 *
 * SUMMARY:
 *   The Hortensian law (287 BCE) established that plebiscita — resolutions of
 *   the plebeian assembly (concilium plebis) — became binding on all Romans,
 *   including patricians and the senate, without requiring senatorial
 *   confirmation. This reading instantiates ONE interpretation of the
 *   contested kernel about popular assemblies and tribunician authority: the
 *   kernel claim is that 'the people's own resolutions constitute binding
 *   law.' The Hortensian law is ONE committer position on this kernel — the
 *   plebeian assembly reading. Sibling readings include the timocratic
 *   centuriate assembly (wealth-organized voting), the contio (persuasion
 *   before voting), and tribunician sacrosanctity (the inviolable tribune as
 *   enforcement mechanism). Each reading grounds the legitimacy of
 *   legislation differently: the centuriate by property class, the contio by
 *   rhetorical authority, the tribunate by collective oath and sacral power,
 *   the plebiscite by plebeian institutional autonomy. This story generates
 *   ONLY the plebiscite reading as a clean ε-invariant constraint. The kernel
 *   contest (which reading legitimizes popular lawmaking?) is routed to omega
 *   variables and cs_structure.reading_relations. The structural delta is
 *   real: patrician legislative monopoly was suppressed; plebeian
 *   institutions became beneficiaries of lawmaking authority; patrician
 *   confirmation rights were eliminated in form (though constrained to
 *   persist through violence, faction, and reserved magistracies).
 *   Extractiveness of class-exclusive lawmaking ended formally but persisted
 *   informationally through non-compliance and domain reservation.
 *
 * KEY AGENTS:
 *   - Plebeian Assembly (concilium plebis): Primary beneficiary (organized/constrained) — gains independent lawmaking authority; benefits from tribunes' enforcement
 *   - Tribunes of the Plebs: Primary beneficiary (institutional/arbitrage) — gain statutory enforcement tool for plebiscita; can arbitrage between plebs and senate
 *   - Plebeian Masses: Victim of patrician monopoly (powerless/trapped) pre-Hortensian; partial beneficiary post-Hortensian (still subject to wealthy plebeian legislation)
 *   - Senate/Patrician Authority: Primary victim (institutional/constrained) — lose legislative monopoly and veto power; constrained to indirect obstruction and violence
 *   - Patrician Class: Institutional victim — reserved magistracies and ius persist but legislative control is eroded
 *   - Magistrates/Magistracy System: Institutional mediator — must now promulgate plebiscita; enforcement mechanisms shift from senatorial confirmation to tribunician coercion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(popular_assemblies_and_tribunate__plebiscite_force_of_law, 0.38).
domain_priors:suppression_score(popular_assemblies_and_tribunate__plebiscite_force_of_law, 0.62).
domain_priors:theater_ratio(popular_assemblies_and_tribunate__plebiscite_force_of_law, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(popular_assemblies_and_tribunate__plebiscite_force_of_law, extractiveness, 0.38).
narrative_ontology:constraint_metric(popular_assemblies_and_tribunate__plebiscite_force_of_law, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(popular_assemblies_and_tribunate__plebiscite_force_of_law, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(popular_assemblies_and_tribunate__plebiscite_force_of_law, tangled_rope).
narrative_ontology:human_readable(popular_assemblies_and_tribunate__plebiscite_force_of_law, "Hortensian Law: Plebiscite Force of Law (Plebeian Lawmaking Without Patrician Confirmation)").
narrative_ontology:topic_domain(popular_assemblies_and_tribunate__plebiscite_force_of_law, "legal/political/constitutional").

domain_priors:requires_active_enforcement(popular_assemblies_and_tribunate__plebiscite_force_of_law).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(popular_assemblies_and_tribunate__plebiscite_force_of_law, '71f1dcc8-8b5c-4ade-b6bd-f710d4720c95').
narrative_ontology:cs_kernel_codification('71f1dcc8-8b5c-4ade-b6bd-f710d4720c95', formalized).
narrative_ontology:cs_authority_grounding('71f1dcc8-8b5c-4ade-b6bd-f710d4720c95', extraction).
narrative_ontology:cs_interpretation_layer_present('71f1dcc8-8b5c-4ade-b6bd-f710d4720c95').
narrative_ontology:cs_reading_relation('71f1dcc8-8b5c-4ade-b6bd-f710d4720c95', popular_assemblies_and_tribunate__comitia_centuriata_timocracy, coexists_with).
narrative_ontology:cs_reading_relation('71f1dcc8-8b5c-4ade-b6bd-f710d4720c95', popular_assemblies_and_tribunate__contio_persuasion_arena, coexists_with).
narrative_ontology:cs_reading_relation('71f1dcc8-8b5c-4ade-b6bd-f710d4720c95', popular_assemblies_and_tribunate__tribunician_sacrosanctity, influences).
narrative_ontology:cs_axiom('71f1dcc8-8b5c-4ade-b6bd-f710d4720c95', foundational, plebeian_institutions_possess_independent_legislative_authority).
narrative_ontology:cs_axiom_status(plebeian_institutions_possess_independent_legislative_authority, holdable).
narrative_ontology:cs_axiom_grounding('71f1dcc8-8b5c-4ade-b6bd-f710d4720c95', plebeian_institutions_possess_independent_legislative_authority, conventional).
narrative_ontology:cs_axiom('71f1dcc8-8b5c-4ade-b6bd-f710d4720c95', foundational, plebiscita_bind_all_romans_including_patricians).
narrative_ontology:cs_axiom_status(plebiscita_bind_all_romans_including_patricians, holdable).
narrative_ontology:cs_axiom_grounding('71f1dcc8-8b5c-4ade-b6bd-f710d4720c95', plebiscita_bind_all_romans_including_patricians, deontological).
narrative_ontology:cs_reference_frame('71f1dcc8-8b5c-4ade-b6bd-f710d4720c95', plebeian_institutional_autonomy_in_lawmaking).
narrative_ontology:cs_drift_state('71f1dcc8-8b5c-4ade-b6bd-f710d4720c95', post_hortensian_compliance_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('71f1dcc8-8b5c-4ade-b6bd-f710d4720c95', '').
narrative_ontology:cs_kernel_id(popular_assemblies_and_tribunate__plebiscite_force_of_law, popular_assemblies_and_tribunate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(popular_assemblies_and_tribunate__plebiscite_force_of_law, plebeian_assembly_institutions).
narrative_ontology:constraint_beneficiary(popular_assemblies_and_tribunate__plebiscite_force_of_law, tribunes_of_the_plebs).
narrative_ontology:constraint_victim(popular_assemblies_and_tribunate__plebiscite_force_of_law, patrician_legislative_confirmation_authority).
narrative_ontology:constraint_victim(popular_assemblies_and_tribunate__plebiscite_force_of_law, senate_patrician_veto_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLEBEIAN MASSES PRE-HORTENSIAN (SNARE) — Trapped by patrician legislative monopoly. No formal lawmaking power; resolutions of the plebeian assembly (concilium plebis) bind only the plebs, not the patricians. Cannot exit the subordinate legal status. Maximum experienced extraction: exclusion from lawmaking authority itself.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__plebiscite_force_of_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PLEBEIAN ASSEMBLY POST-HORTENSIAN (TANGLED ROPE) — Constrained by enforced compliance mechanisms (tribunes enforce plebiscita) and political resistance from patrician factions. But also genuinely coordinates plebeian interests through collective voting. Extractiveness shifted from total exclusion to partial asymmetry: plebs now make law but within limits (certain reserved magistracies remain patrician; patrician ius remain untouched in some domains). Mixed coordination and extraction.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__plebiscite_force_of_law, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TRIBUNICIAN AUTHORITY (ROPE) — Sees plebiscite force as coordination mechanism: tribunes now have statutory tool to enforce plebeian legislation. Coordinates the plebs' collective action into binding law. Net beneficiary through institutional power expansion. Can arbitrage between plebeian factions and senate.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__plebiscite_force_of_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SENATE/PATRICIAN AUTHORITY (SNARE) — Constrained by erosion of legislative monopoly. Cannot veto plebiscita anymore; confirmation authority suppressed. Extraction runs away from this perspective: patrician oligarchic control over law production is now limited. High suppression of patrician veto mechanisms; constrained alternatives (filibuster through tribunes, procedural obstruction, violence).
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__plebiscite_force_of_law, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL CONSTITUTION / THEORETICAL VIEW (PITON) — The written constitutional order claims that popular sovereignty is the source of all legitimate law. But in practice, the plebeian assembly and senate operate in parallel, not as unified sovereign body. The constitutional claim (plebeian plebiscita are binding law) is partly performative: actual compliance depends on tribunes enforcing, on absence of patrician violence, on whether the plebs hold to their oath. Theater ratio reflects the gap between constitutional claim and institutional reality.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__plebiscite_force_of_law, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the Hortensian law appears as an inevitable consequence of popular power: once a large subaltern population organizes with institutional representatives (tribunes), some form of independent legislative capacity becomes structurally necessary. The law was not contingent but emergent. However, the structural data contradicts this — the patricians actively resisted, violence occurred, the law required specific coalition-building (Hortensius' position as censor). This is a false summit: naturalization of what was a contingent political victory.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__plebiscite_force_of_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(popular_assemblies_and_tribunate__plebiscite_force_of_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__plebiscite_force_of_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__plebiscite_force_of_law, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(popular_assemblies_and_tribunate__plebiscite_force_of_law, TR),
    TR >= 0.70.

:- end_tests(popular_assemblies_and_tribunate__plebiscite_force_of_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.38 post-Hortensian (vs 0.85 pre-Hortensian). The constraint represents a substantial reduction in class-exclusive lawmaking extraction. However, 0.38 reflects that extractiveness did not disappear: (1) Wealthy plebs within the assembly could legislate against poor plebs without patrician counterveto; (2) Reserved domains (consulate, censorship, augury) remained patrician monopolies; (3) Compliance remained contingent on plebeian organizing capacity — if plebs fractured, patricians could obstruct plebiscita through violence or faction. So the constraint is Tangled Rope, not pure Rope: it coordinates plebeian legislative interests but retains asymmetric extraction through domain reservation and class stratification within the plebs. Suppression: 0.62 post-Hortensian (vs 0.45 pre-Hortensian). Paradoxically, suppression increased during the transition period (0.68 at t=5) because enforcement of the new law required coercive tribunician action against patrician resistance — building the enforcement apparatus added friction. By t=10, suppression stabilized at 0.62 (still higher than pre-Hortensian 0.45) because plebiscita require: tribunes enforcing, plebs maintaining solidarity, avoidance of patrician violence. Theater ratio: 0.55 post-Hortensian. The contio (persuasion assembly) pre-Hortensian was highly performative (0.40 theater) — rhetorical display before voting. Post-Hortensian, plebiscite voting added formal ritual (tribunician promulgation, oath-taking, formal vote recording) but also genuine legislative consequence (laws were enacted). Theater increased because proceduralism added layers, but fell short of pure performance (piton level) because outcomes were materially binding.
 *
 * PERSPECTIVAL GAP:
 *   Six perspectives produce a full spectrum of DR types. The plebeian masses pre-Hortensian saw Snare (total exclusion from lawmaking). The plebeian assembly post-Hortensian sees Tangled Rope (genuine coordination with residual asymmetry). The tribunes see Rope (pure coordination of plebeian power into legislative instrument). The senate sees Snare (erosion of monopoly, suppression of veto, constrained to obstructionist alternatives). The formal constitution sees Piton (constitutional claim versus institutional reality — the gap between 'plebiscita are binding' and 'compliance depends on tribunes' enforcement'). The analytical observer at civilizational scope risks seeing Mountain (inevitable emergence of popular legislative power) — but the structural data (patrician resistance, violence, domain reservation, wealth stratification within plebs) contradicts this. The engine's false summit detector identifies this as naturalization of contingent political victory. The perspectival gap reveals that the same law appears as liberation (plebeian), coordination (tribunician), constraint (senatorial), ritual performance (constitutional), and inevitable law of nature (analytical) depending on the observer's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim status and exit options. Plebeian assembly (organized/constrained) are beneficiaries with constrained exit (cannot exit without losing institutional power) — derives d around 0.35-0.45, moderate f(d). Tribunes (institutional/arbitrage) are beneficiaries with arbitrage exit (can mediate between plebs and senate) — derives d around 0.20, low f(d), low or negative χ from their perspective. Senate (institutional/constrained) are victims with constrained exit (cannot exit without abandoning authority) — derives d around 0.65-0.75, high f(d). Plebeian masses (powerless/trapped) pre-Hortensian are victims with trapped exit — derives d near 1.0, maximum f(d). Post-Hortensian, plebeian masses have shifted toward constrained exit (can now participate in assembly voting) but remain subject to wealthy plebs' will — derives d around 0.70, high f(d). The perspectival gaps in classification (Snare vs Tangled Rope vs Rope) reflect these directionality differences: high d (victims) classifies as Snare; moderate d with mixed benefits (tribunes) as Rope; moderate-high d with mixed coordination/extraction (assembly) as Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The Hortensian law constraint resolves the mandatrophy by revealing that the 'inevitable emergence of popular legislative power' (mountain perspective) is actually a contingent political victory embedded in specific institutional arrangements (tangled rope / snare / piton from other perspectives). The classical oracle gap is operative: the analytical observer sees formal law (plebiscita are binding) but cannot detect from the universal-scope view that compliance is maintained by the tribunes' enforcement power and the plebs' maintaining solidarity. The piton perspective reveals the constitutional claim ('plebiscita are law') performs more than it accomplishes — the gap between form and function is the constraint's real structure. The mandatrophy resolution: all six classifications are valid. The mountain is false — not because plebiscita aren't law, but because the lawfulness is constructed through institutional arrangements (tribunician enforcement, plebeian oath) that could fail. The snare and tangled_rope classifications are real from plebeian/patrician perspectives. The rope is real from the tribunician perspective. The piton is real from the constitutional perspective. The constraint is not 'which type is correct' but 'which perspective are we measuring from,' and the presheaf over the observation site encodes the full structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_compliance_vs_formal_binding,
    'Did plebiscita actually bind patricians and the senate post-Hortensian, or was compliance contingent on plebeian organization and threat capacity?',
    'Historical record of compliance: count instances where senate/patricians obeyed plebiscita vs. instances of obstruction, nullification, or violent reversal. Trace relationship between tribunes'' actual enforcement power and compliance rates.',
    'If binding: mountain/rope classification confirmed — formal law functioned. If contingent: snare/tangled_rope classification — extraction persists through non-compliance. If mixed: tangled_rope confirmed with measurement of the compliance threshold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(actual_compliance_vs_formal_binding, empirical, 'Whether plebiscita were actually binding post-Hortensian or remained contingent on plebeian enforcement').

omega_variable(
    plebeian_solidarity_fracture,
    'Did plebiscite lawmaking authority fragment plebeian unity by enabling wealthy plebs to legislate against poor plebs without patrician counterweight?',
    'Analysis of plebiscita content and voting patterns post-Hortensian: correlation between wealth classes within plebs and plebiscita outcomes. Instances where wealthy plebs used plebiscite to extract from poor plebs without aristocratic interference.',
    'If fragmented: the Hortensian law enabled class extraction within the plebs — externally it ended patrician-plebeian extraction but internally created plebeian oligarchy. Extractiveness reassesses upward for poor plebs'' perspective. If unified: solidarity persisted and extraction was genuinely reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plebeian_solidarity_fracture, empirical, 'Whether plebiscite authority created internal plebeian class extraction').

omega_variable(
    kernel_reading_contest_plebeian_law,
    'Is the Hortensian law''s binding force a reading of a contested kernel about popular sovereignty and legislative authority, or an outcome of military/political power struggle?',
    'Examine the committer positions: senators (lineage authority), tribunes (sacrosanctity authority), plebeian assembly (practice authority), magistrates mediating (expertise/persuasion authority). Each grounds the law''s legitimacy differently. The kernel is the claim that ''the people''s resolutions are law'' — this reading instantiates one committer position among several.',
    'If kernel reading: the Hortensian law is one stable interpretation of popular sovereignty; sibling readings (comitia centuriata, tribunician sacrosanctity, contio persuasion) represent alternative groundings. If outcome of power struggle: the law is contingent, subject to reversal if power relations shift. Classification unchanged but frame shifts from constitutive to political.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_plebeian_law, conceptual, 'Whether Hortensian law is a kernel reading vs. outcome of power dynamics').

omega_variable(
    reserved_patrician_domains_extent,
    'What was the actual scope of domains explicitly reserved to patrician magistracies or ius post-Hortensian? Did plebiscita legally bind in all domains or only certain areas?',
    'Legal analysis of patrician ius residue: consulate (exclusively patrician until 367), censorship (patrician monopoly), augury (patrician privilege). Count plebiscita that touched these domains vs. those that did not. Measure extractiveness separately for each domain.',
    'If broad reservation: extractiveness of class-exclusive lawmaking persisted in core domains; tangled_rope classification confirmed with domain-specific ε values. If minimal reservation: extractiveness dropped and constraint approached rope (pure coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reserved_patrician_domains_extent, empirical, 'Extent of legal domains reserved to patrician authority post-Hortensian').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(popular_assemblies_and_tribunate__plebiscite_force_of_law, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plebisc_theater_pre_hortensian_contio_dominant, popular_assemblies_and_tribunate__plebiscite_force_of_law, theater_ratio, 0, 0.4).
narrative_ontology:measurement(plebisc_theater_transition_enforcement_ritual, popular_assemblies_and_tribunate__plebiscite_force_of_law, theater_ratio, 5, 0.58).
narrative_ontology:measurement(plebisc_theater_post_hortensian_stabilized, popular_assemblies_and_tribunate__plebiscite_force_of_law, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(plebisc_extract_pre_hortensian, popular_assemblies_and_tribunate__plebiscite_force_of_law, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(plebisc_extract_transition_period, popular_assemblies_and_tribunate__plebiscite_force_of_law, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(plebisc_extract_post_hortensian_stabilized, popular_assemblies_and_tribunate__plebiscite_force_of_law, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(plebisc_suppress_pre_hortensian, popular_assemblies_and_tribunate__plebiscite_force_of_law, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(plebisc_suppress_transition_enforcement_buildup, popular_assemblies_and_tribunate__plebiscite_force_of_law, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(plebisc_suppress_post_hortensian_stabilized, popular_assemblies_and_tribunate__plebiscite_force_of_law, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(popular_assemblies_and_tribunate__plebiscite_force_of_law, enforcement_mechanism).
narrative_ontology:affects_constraint(popular_assemblies_and_tribunate__plebiscite_force_of_law, popular_assemblies_and_tribunate__comitia_centuriata_timocracy).
narrative_ontology:affects_constraint(popular_assemblies_and_tribunate__plebiscite_force_of_law, popular_assemblies_and_tribunate__tribunician_sacrosanctity).
narrative_ontology:affects_constraint(popular_assemblies_and_tribunate__plebiscite_force_of_law, popular_assemblies_and_tribunate__contio_persuasion_arena).

% DUAL FORMULATION NOTE:
% The plebiscite_force_of_law reading is one element of a contested kernel about popular lawmaking in Rome. The kernel_id (popular_assemblies_and_tribunate) links four constraint stories representing four different committer positions on how legitimate legislation is grounded: property-class voting (centuriate), rhetorical persuasion (contio), inviolable tribune-person (sacrosanctity), and plebeian institutional autonomy (plebiscite). Each story has its own ε and perspectival gap. This story (plebiscite) affects the others by constraining their scope: the centuriate assembly and contio are no longer the sole sovereign bodies once plebiscita are binding; tribunician veto gains new force through plebeian backing. The sibling readings remain live (different factions hold different interpretations), but their structural position shifts after the Hortensian law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
