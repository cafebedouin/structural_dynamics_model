% ============================================================================
% CONSTRAINT STORY: popular_assemblies_and_tribunate__tribunician_sacrosanctity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tribunician_sacrosanctity, []).

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
 *   constraint_id: popular_assemblies_and_tribunate__tribunician_sacrosanctity
 *   human_readable: Tribunician Sacrosanctity: The Veto as Corporeal Shield
 *   domain: legal/constitutional/Roman
 *
 * SUMMARY:
 *   The tribune rests on a formal paradox: an elected magistrate whose power
 *   is defined by what cannot be done to him. The Republic produces tribunes
 *   to protect plebeians from arbitrary arrest and execution by consuls. The
 *   protection works not through the tribune's positive commands but through
 *   his negative veto and his inviolable body. If a consul attempts to coerce
 *   a plebeian, the tribune interposes his person—not by drawing a sword but
 *   by occupying the space of violation. To touch the tribune is to violate
 *   the collective oath of the plebs. The mechanism is both legal (the veto
 *   is formal, recorded, institutionalized) and sacred (the inviolability
 *   rests on oath, on religious sanction, on the plebs' united will to treat
 *   any violation as catastrophic). This constraint shows the full spectrum
 *   of indexical classification because different agents experience the veto
 *   mechanism radically differently. For a plebeian under a consul's
 *   coercion, it is Snare—they are trapped, their only exit a tribune's
 *   willingness to risk his body. For the plebeian collective that sustains
 *   the oath, it is Tangled Rope—genuine protection coupled with costs of
 *   organization and moral hazard. For the consul whose imperium is blocked,
 *   it is Snare—their traditional authority is suppressed. For the tribunes
 *   themselves, it is Rope—they solve the coordination problem that
 *   magistrates and plebs would otherwise resolve through violence. For later
 *   emperors, the title persists as Piton—the theater of tribunician power
 *   without the functional veto.
 *
 * KEY AGENTS:
 *   - Plebeian defendant: victim of magisterial coercion, primary beneficiary of the veto (trapped/powerless)
 *   - Plebeian collective: sustains the sacred oath, both benefits (protection) and pays (organizational costs) (moderate/organized)
 *   - Consul/Magistrate: experiences veto as suppression of traditional imperium; blocked from arbitrary coercion (powerful/mobile)
 *   - Tribune: elected magistrate whose power is defined by inviolability and veto; moral hazard (organized/constrained)
 *   - Patrician oligarchy: loses exclusive monopoly on enforcement authority; checks on their coercive power increase (powerful/arbitrage)
 *   - Analytical observer: recognizes this as one reading of a contested kernel; other readings prioritize voting structure, plebiscite force, or rhetorical persuasion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(popular_assemblies_and_tribunate__tribunician_sacrosanctity, 0.38).
domain_priors:suppression_score(popular_assemblies_and_tribunate__tribunician_sacrosanctity, 0.62).
domain_priors:theater_ratio(popular_assemblies_and_tribunate__tribunician_sacrosanctity, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(popular_assemblies_and_tribunate__tribunician_sacrosanctity, extractiveness, 0.38).
narrative_ontology:constraint_metric(popular_assemblies_and_tribunate__tribunician_sacrosanctity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(popular_assemblies_and_tribunate__tribunician_sacrosanctity, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(popular_assemblies_and_tribunate__tribunician_sacrosanctity, tangled_rope).
narrative_ontology:human_readable(popular_assemblies_and_tribunate__tribunician_sacrosanctity, "Tribunician Sacrosanctity: The Veto as Corporeal Shield").
narrative_ontology:topic_domain(popular_assemblies_and_tribunate__tribunician_sacrosanctity, "legal/constitutional/Roman").

domain_priors:requires_active_enforcement(popular_assemblies_and_tribunate__tribunician_sacrosanctity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(popular_assemblies_and_tribunate__tribunician_sacrosanctity, 'e6892f67-f98c-47b5-98ac-f1bb35031e1b').
narrative_ontology:cs_kernel_codification('e6892f67-f98c-47b5-98ac-f1bb35031e1b', fixed_text).
narrative_ontology:cs_authority_grounding('e6892f67-f98c-47b5-98ac-f1bb35031e1b', extraction).
narrative_ontology:cs_interpretation_layer_present('e6892f67-f98c-47b5-98ac-f1bb35031e1b').
narrative_ontology:cs_reading_relation('e6892f67-f98c-47b5-98ac-f1bb35031e1b', popular_assemblies_and_tribunate__comitia_centuriata_timocracy, coexists_with).
narrative_ontology:cs_reading_relation('e6892f67-f98c-47b5-98ac-f1bb35031e1b', popular_assemblies_and_tribunate__contio_persuasion_arena, influences).
narrative_ontology:cs_reading_relation('e6892f67-f98c-47b5-98ac-f1bb35031e1b', popular_assemblies_and_tribunate__plebiscite_force_of_law, coexists_with).
narrative_ontology:cs_axiom('e6892f67-f98c-47b5-98ac-f1bb35031e1b', foundational, inviolable_person_as_legal_shield).
narrative_ontology:cs_axiom_status(inviolable_person_as_legal_shield, holdable).
narrative_ontology:cs_axiom_grounding('e6892f67-f98c-47b5-98ac-f1bb35031e1b', inviolable_person_as_legal_shield, deontological).
narrative_ontology:cs_axiom('e6892f67-f98c-47b5-98ac-f1bb35031e1b', foundational, oath_as_collective_binding_mechanism).
narrative_ontology:cs_axiom_status(oath_as_collective_binding_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('e6892f67-f98c-47b5-98ac-f1bb35031e1b', oath_as_collective_binding_mechanism, conventional).
narrative_ontology:cs_reference_frame('e6892f67-f98c-47b5-98ac-f1bb35031e1b', plebeian_protection_through_legal_veto).
narrative_ontology:cs_drift_state('e6892f67-f98c-47b5-98ac-f1bb35031e1b', late_republic_and_transition_to_principate, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('e6892f67-f98c-47b5-98ac-f1bb35031e1b', '').
narrative_ontology:cs_kernel_id(popular_assemblies_and_tribunate__tribunician_sacrosanctity, popular_assemblies_and_tribunate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(popular_assemblies_and_tribunate__tribunician_sacrosanctity, plebeian_defendant).
narrative_ontology:constraint_beneficiary(popular_assemblies_and_tribunate__tribunician_sacrosanctity, plebeian_collective).
narrative_ontology:constraint_victim(popular_assemblies_and_tribunate__tribunician_sacrosanctity, magisterial_imperium).
narrative_ontology:constraint_victim(popular_assemblies_and_tribunate__tribunician_sacrosanctity, patrician_enforcement_monopoly).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLEBEIAN FACING CONSUL'S IMPERIUM (SNARE) — A plebeian arrested by a consul experiences maximum suppression and no exit. The tribunal's veto blocks coercion but only in a single moment; the plebeian cannot exit the constraint itself. The tribune's body is a shield, not liberation. Once the shield is removed or the tribune silenced, the imperium resumes without barrier. Trapped in the Republic's enforcement structure; saved only by continuous veto.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__tribunician_sacrosanctity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PLEBEIAN COLLECTIVE AS TRIBUNE'S CONSTITUENCY (TANGLED ROPE) — The plebs collectively benefit from the veto mechanism (genuine coordination: the tribune communicates plebeian will to magistrates). They also bear extraction: maintaining the sanctity requires continuous oath-swearing, organizational costs, and exposure to retaliation if the tribune is perceived as too bold. The collective can organize (the oath itself is an organized act) but cannot exit the need for protection without restoring magistrate discretion. Mixed coordination and extraction.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__tribunician_sacrosanctity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PATRICIAN CONSUL FACING VETO AUTHORITY (SNARE) — The consul experiences the veto as pure constraint with no coordination benefit. The magistrate's traditional imperium is blocked by the tribune's inviolable body. Exit exists only at political cost (refusing to take office, or accepting demotion). The consul sees extraction: the plebeian veto transfers power from the executive branch to the tribunes without magisterial consent or institutional quid pro quo. No genuine coordination is present from this perspective — only suppression of the magistrate's natural authority.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__tribunician_sacrosanctity, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: TRIBUNES AS COORDINATE INSTITUTION (ROPE) — From the tribunate's own institutional perspective (over generations), sacrosanctity solves a genuine coordination problem: how do the plebs check magisterial excess without abolishing imperium or collapsing the Republic? The veto coordinate between orders. The sacrifice of the tribune (personal exposure, moral hazard, potential assassination) is the price of this coordination. The tribunes see their role as functionally necessary, not extractive. Theater is moderate because the veto mechanism actually works — the threat is credible.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__tribunician_sacrosanctity, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PRINCIPATE REUSING TRIBUNICIAN SACROSANCTITY AS TITLE (PITON) — By the Principate, the emperor claims 'tribunician power' (potestas tribunicia) as a title and formal grant from the senate, but the veto mechanism no longer functions as a plebeian check — it has become a formal designation of the emperor's immunity from legal challenge. The original sacrosanctity (pleb protection) persists as institutional theater; the functional veto is gone. The ritual is maintained through inertia and legitimacy inheritance, not because tribunes any longer coordinate plebeian power. Theater ratio high; functional constraint low.
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__tribunician_sacrosanctity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — READING-SPECIFIC (TANGLED ROPE) — The analytical observer recognizes this reading as one instantiation of a contested kernel. The tribunician sacrosanctity reading embeds a specific claim about how plebeian power is channeled: through the protections of law (the inviolable person) rather than through the direct legislative force of plebiscites (the plebiscite reading) or through the institutional voting structure (the centuriate reading). This reading privileges the rule-of-law mechanism (veto as a juridical shield) over alternative mechanisms. The constraint is tangled rope because it combines genuine coordination (plebs + magistrates reaching a modus vivendi) with extraction (the plebs must continuously re-authorize the tribune's sacred status, incurring costs of organization and moral hazard).
constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__tribunician_sacrosanctity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(popular_assemblies_and_tribunate__tribunician_sacrosanctity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__tribunician_sacrosanctity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(popular_assemblies_and_tribunate__tribunician_sacrosanctity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(popular_assemblies_and_tribunate__tribunician_sacrosanctity, TR),
    TR >= 0.70.

:- end_tests(popular_assemblies_and_tribunate__tribunician_sacrosanctity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The veto mechanism suppresses magisterial coercion but does not eliminate it—only delays or prevents the most arbitrary acts. The plebeian must still present himself before the magistrate; he has lost immediate freedom of action; his exit is conditional on the tribune's willingness to interpose. The magistrate loses unilateral enforcement power but retains command of the military and police apparatus. This is not pure coordination (Rope) because the veto asymmetrically benefits plebeians at the cost of patrician authority. It is not pure extraction (Snare) because the plebs gain genuine protection from arbitrary violence. The extractiveness rises slightly over the Republican period (0.28 → 0.45) as the tribunes' powers expand beyond protecting arrested individuals to broader legislative and electoral functions. Suppression (0.62): Moderate-high and rising. The mechanism depends on the plebeian oath remaining credible and cohesive. If the plebs fracture or the oath loses force, magistrates can resume arbitrary coercion. The magistrate is suppressed: unable to execute his traditional authority without risk of triggering plebeian secession or violence. This suppression becomes more severe (0.50 → 0.68) as the tribunes' veto reaches to more domains of governance. Theater ratio (0.55 → 0.62): Moderate to moderate-high. The veto mechanism is partially performative—much of its force rests on credible threat (the tribune's willingness to stand firm, the plebs' willingness to back the oath). As the Republic ages and tribunes multiply, the ritual element increases (theater ratio rises) while functional effectiveness may decline. The Principate will preserve the title while hollowing the mechanism (theater → 0.85+, function → near zero).
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces radical perspectival divergence across agents. The plebeian defendant sees Snare (maximum suppression, no exit except through the tribune's body). The plebeian collective sees Tangled Rope (genuine coordination gain mixed with organizational burden). The consul sees Snare (their authority suppressed without reciprocal gain). The tribune sees Rope (solving a genuine coordination problem). The later emperor sees Piton (the theater of power without function). The analytical observer sees Tangled Rope (coordination mechanism with asymmetric extraction). This is not uncertainty about which classification is 'correct'—it is genuine structural difference in how agents experience the mechanism. The classification type varies because the agents' power, exit options, and beneficiary/victim status differ. The perspectival gap is the feature, not a bug—it reveals how a single institutional mechanism can embed multiple, incompatible experiences of power.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) encodes each agent's structural relationship to the constraint. A plebeian defendant who benefits from protection has low d (around 0.15–0.25, beneficiary); the plebeian collective that sustains the oath has moderate d (around 0.45–0.55, both beneficiary and victim). A consul whose authority is suppressed has high d (around 0.75–0.85, victim of the veto mechanism). The tribunes, who benefit from their role as necessary coordinators but face assassination risk, have moderate d (around 0.50, symmetric cost-benefit). The analytical observer's d is around 0.72 (observer position, independent of beneficiary/victim status). These directional values feed the sigmoid f(d) to produce effective extractiveness chi = ε × f(d) × σ(S). A beneficiary's negative effective extraction reflects that this agent experiences the constraint as a gain. A victim's positive effective extraction reflects that this agent experiences it as a cost. The divergence in chi across perspectives is what produces the perspectival gap in classification type.
 *
 * MANDATROPHY ANALYSIS:
 *   The tribunician sacrosanctity reading avoids mandatrophy by recognizing that the constraint is genuinely Tangled Rope: it coordinates plebeian-magistrate relations AND extracts from magistrates' traditional prerogatives. The plebs gain protection; the magistrates lose autonomy. No perspective sees the constraint as pure coordination (except the tribunes themselves, whose interest is to naturalize their own necessity). No perspective sees it as pure extraction except from the victim magistrate's view. The analytical observer correctly classifies as Tangled Rope because both elements are structurally present: the veto solves a real coordination problem (magistrate cannot unilaterally use coercion without plebeian response), AND it transfers power from magistrates to tribunes/plebs. The constraint is not hiding as a mountain (natural law) or a rope (pure coordination)—it is an overtly constructed mechanism of constitutional compromise, and the classification reflects this. The risk of mandatrophy would arise if the analytical observer tried to naturalize the veto as an immutable feature of law or human nature (false mountain), or if a beneficiary tried to frame it as pure coordination with no extraction costs (rope). The framework prevents both by insisting on the structural data: beneficiaries + victims + enforcement = Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacrosanctity_enforcement_mechanism,
    'Is the tribunal''s veto enforced by the collective plebeian oath (internal legitimacy) or by the threat of secession/rebellion (external coercion)?',
    'Historical analysis of instances where the veto was challenged: did the threat of secession stop the magistrate, or did the magistrate respect the oath itself? Examination of the Licinio-Sextian reforms and later tribune conflicts.',
    'If oath-based: the constraint is a coordination mechanism sustained by normative consensus (Rope from more perspectives). If secession-based: it is enforcement by threat (Snare/Tangled Rope from all perspectives). The distinction affects whether suppression is structural (external barrier) or internalized (collective commitment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacrosanctity_enforcement_mechanism, empirical, 'Whether veto enforcement rests on oath or secession threat').

omega_variable(
    tribunician_veto_scope,
    'Does the tribune''s veto apply to all magisterial actions, or only specific classes (arrest, enforcement of judgments, execution)? Is the veto scope expanding or contracting over the Republican period?',
    'Textual analysis of the Twelve Tables and Cicero''s accounts; reconstruction of vetoed actions in extant records; comparison of early and late Republican tribunal powers.',
    'If veto is narrow (e.g., only arrest): the constraint protects a specific vulnerability (arbitrary detention) without broadly constraining magistrates (Rope classification holds). If veto is broad (any magisterial act): the constraint severely restricts executive power (Snare from magistrate perspective). Expanding scope indicates escalating conflict; contracting scope indicates compromise or weakening.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tribunician_veto_scope, empirical, 'Scope and temporal trajectory of tribunician veto authority').

omega_variable(
    reading_vs_comitia_centuriata_contrast,
    'Does the tribunician sacrosanctity reading foreclose, coexist with, or influence the comitia_centuriata_timocracy reading? If the centuriate assembly votes by wealth class (giving patricians structural advantage), does the tribune''s veto provide a secondary check, or do the two mechanisms contradict?',
    'Historical reconstruction of plebeian influence pathways: did tribunes function to overcome voting inequalities in the centuriate assembly, or did they operate in a separate sphere (veto of execution/enforcement)? Analysis of Livy and Polybius on inter-institutional checks.',
    'If coexists: both mechanisms operate independently; plebs have multiple checks against patrician power. If influences: tribunal primarily constrains executive enforcement (not legislative voting), so centuriate voting structure remains intact. If forecloses: the presence of an effective veto would make wealth-weighted voting redundant or non-functional (rare logical conclusion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_comitia_centuriata_contrast, conceptual, 'Relationship between tribunician veto and centuriate voting structure').

omega_variable(
    sacred_vs_legal_distinction,
    'Is the tribune''s inviolability grounded in sacred oath (religious, collective, revocable) or in positive law (formal, institutionalized, binding)? Does the Twelve Tables enshrine sacrosanctity as law, or does it rest on continuous renewal of the plebeian oath?',
    'Textual examination of the Twelve Tables and Cicero''s citations; determination of whether the oath is framed as a renewable social contract or a binding legal institution.',
    'If sacred/oath-based: the constraint is structurally fragile—it depends on continuous plebeian commitment and can evaporate if the oath fractures (tea-leaves reading). If legal/institutionalized: the constraint is durable, embedded in the Republic''s formal law, less vulnerable to political shifts. The distinction affects assessed theater ratio (oath-based = higher theater; legal = lower theater).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacred_vs_legal_distinction, empirical, 'Sacred oath vs. positive law grounding for tribunician inviolability').

omega_variable(
    tribune_assassination_and_constraint_failure,
    'When tribunes are assassinated (Tiberius Gracchus, Gaius Gracchus, Sulpicius Rufus), does the sacrosanctity constraint fail structurally, or does the violation confirm the constraint''s importance by its breach?',
    'Historical analysis of tribune assassinations: did the killing end the constraint, strengthen plebeian resolve, or trigger constitutional reform? Were perpetrators punished or exonerated?',
    'If failures: sacrosanctity is fragile, revocable, and ultimately unenforceable (ε increases; suppression of the constraint by patrician force becomes visible). If confirmatory violations: the sacredness is proven by the catastrophic consequences of violation (ε stable; suppression of tribunes as a class increases but sacrosanctity as a principle is reaffirmed). This determines whether the constraint is a mountain-like immutable feature (violation triggers systemic collapse) or a Rope/Tangled Rope that can be overridden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tribune_assassination_and_constraint_failure, empirical, 'Whether tribune assassination indicates constraint failure or constraint sacralization').

omega_variable(
    kernel_committer_structural_ambiguity,
    'Does the tribunician sacrosanctity reading represent the kernel of plebeian power (the binding mechanism that makes plebeian will enforceable), or is it one alternative reading of how plebeian power is channeled, with plebiscite force and voting structure as rival mechanisms?',
    'Meta-analysis of the kernel contest: examine whether sacrosanctity is claimed as the CORE protection by Roman sources, or whether it is described as coordinate with plebiscite authority and voting participation. Livy''s narrative arc: does he treat the tribunal as foundational or supplementary?',
    'If sacrosanctity is the core: other readings (plebiscite, voting structure) are derivative or supplementary mechanisms. If it is one alternative: sacrosanctity, plebiscite, and voting structure are genuinely independent channels, and the reading relationships (coexists, influences) are symmetrical rather than hierarchical. This affects whether the constraint is best classified as Rope (coordinate mechanism) or Snare/Tangled Rope (competing power-structures that constrain each other).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_structural_ambiguity, conceptual, 'Whether tribunician sacrosanctity is the kernel''s core or one alternative reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(popular_assemblies_and_tribunate__tribunician_sacrosanctity, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trib_sac_theater_early_republic, popular_assemblies_and_tribunate__tribunician_sacrosanctity, theater_ratio, 0, 0.4).
narrative_ontology:measurement(trib_sac_theater_middle_republic, popular_assemblies_and_tribunate__tribunician_sacrosanctity, theater_ratio, 200, 0.52).
narrative_ontology:measurement(trib_sac_theater_late_republic, popular_assemblies_and_tribunate__tribunician_sacrosanctity, theater_ratio, 400, 0.62).

% Extraction over time
narrative_ontology:measurement(trib_sac_extract_early_republic, popular_assemblies_and_tribunate__tribunician_sacrosanctity, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(trib_sac_extract_middle_republic, popular_assemblies_and_tribunate__tribunician_sacrosanctity, base_extractiveness, 200, 0.38).
narrative_ontology:measurement(trib_sac_extract_late_republic, popular_assemblies_and_tribunate__tribunician_sacrosanctity, base_extractiveness, 400, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(trib_sac_suppress_early_republic, popular_assemblies_and_tribunate__tribunician_sacrosanctity, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(trib_sac_suppress_middle_republic, popular_assemblies_and_tribunate__tribunician_sacrosanctity, suppression_requirement, 200, 0.62).
narrative_ontology:measurement(trib_sac_suppress_late_republic, popular_assemblies_and_tribunate__tribunician_sacrosanctity, suppression_requirement, 400, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(popular_assemblies_and_tribunate__tribunician_sacrosanctity, enforcement_mechanism).
narrative_ontology:affects_constraint(popular_assemblies_and_tribunate__tribunician_sacrosanctity, comitia_centuriata_timocracy).
narrative_ontology:affects_constraint(popular_assemblies_and_tribunate__tribunician_sacrosanctity, plebiscite_force_of_law).
narrative_ontology:affects_constraint(popular_assemblies_and_tribunate__tribunician_sacrosanctity, contio_persuasion_arena).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'popular_assemblies_and_tribunate'. The sibling readings (comitia_centuriata_timocracy, plebiscite_force_of_law, contio_persuasion_arena) are structurally independent constraints with their own ε values and beneficiary/victim structures. The sacrosanctity reading emphasizes the protection mechanism (veto, inviolable person, oath). The plebiscite reading emphasizes legislative sovereignty (binding plebeian resolutions). The centuriate reading emphasizes voting structure (wealth-weighted voting that sometimes benefited plebeians). The contio reading emphasizes rhetorical persuasion (the public sphere where magistrates faced plebeian opinion). Each reading isolates a different structural mechanism for channeling plebeian power. They coexist historically; none forecloses the others; each may influence resource allocation and institutional development in ways that affect the others' operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(popular_assemblies_and_tribunate__tribunician_sacrosanctity, powerful, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
