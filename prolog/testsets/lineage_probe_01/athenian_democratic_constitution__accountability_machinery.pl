% ============================================================================
% CONSTRAINT STORY: athenian_democratic_constitution__accountability_machinery
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_athenian_democratic_constitution__accountability_machinery, []).

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
 *   constraint_id: athenian_democratic_constitution__accountability_machinery
 *   human_readable: Athenian Accountability Machinery: Audits, Liability, and Ostracism
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   Athenian democracy enshrined the principle that power must be scrutinized
 *   and officials held personally liable. Every archon, general, treasurer,
 *   and proposer faced mandatory examination upon leaving office (euthynai).
 *   Those who violated the law faced prosecution (graphe paranomon for
 *   unconstitutional decrees, eisangelia for treason or corruption).
 *   Ostracism — the democratic power to exile by popular vote — served as a
 *   final brake on whoever 'grew too large,' accumulating excessive
 *   influence. This accountability machinery is a core institutional feature
 *   of Athenian democracy, but it is NOT the constitution itself — it is ONE
 *   reading of what the constitution IS. Other readings emphasize the
 *   assembly's supremacy, the exclusionary basis in slavery and metic
 *   subjugation, or the role of sortition and rotation. This constraint story
 *   instantiates the accountability-machinery reading: Athens' constitution
 *   is its audits. The constraint models how this reading generates a
 *   specific structure of beneficiaries (the demos, controlling agents),
 *   victims (officials bearing liability), and costs (suppression of
 *   unaccountable service, extraction of personal risk from those who
 *   govern). The historical measurement trajectory shows that extractiveness
 *   rose slightly (0.32 → 0.42) and theater ratio increased substantially
 *   (0.25 → 0.52) over the classical period, reflecting degradation from
 *   genuine constraint to increasingly ritualistic form — a pattern
 *   consistent with the piton diagnosis from a civilizational perspective.
 *
 * KEY AGENTS:
 *   - Demos (assembled citizens): Primary beneficiary (institutional/arbitrage) — controls accountability mechanisms; defines rules; cannot be externally constrained
 *   - Office-holders and magistrates: Primary victims (powerless/trapped for those in office; organized/constrained for the magistrate class) — bear personal liability, audit examination, risk of ostracism or prosecution
 *   - Legislative proposers (orators): Secondary victims (powerful/constrained) — face graphe paranomon for unconstitutional decrees; bear burden of defending their proposals
 *   - Logistai (audit examiners): Institutional actors (institutional/arbitrage) — implement accountability machinery; possess discretion over what counts as violation
 *   - Sycophants (prosecutors): Organized actors (organized/constrained) — profit from prosecutions; drive enforcement but also distort it toward factional conflict
 *   - Analytical observer: External viewpoint (analytical/analytical) — risks naturalizing contingent institutional design as structural necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(athenian_democratic_constitution__accountability_machinery, 0.38).
domain_priors:suppression_score(athenian_democratic_constitution__accountability_machinery, 0.52).
domain_priors:theater_ratio(athenian_democratic_constitution__accountability_machinery, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(athenian_democratic_constitution__accountability_machinery, extractiveness, 0.38).
narrative_ontology:constraint_metric(athenian_democratic_constitution__accountability_machinery, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(athenian_democratic_constitution__accountability_machinery, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(athenian_democratic_constitution__accountability_machinery, tangled_rope).
narrative_ontology:human_readable(athenian_democratic_constitution__accountability_machinery, "Athenian Accountability Machinery: Audits, Liability, and Ostracism").
narrative_ontology:topic_domain(athenian_democratic_constitution__accountability_machinery, "political/constitutional").

domain_priors:requires_active_enforcement(athenian_democratic_constitution__accountability_machinery).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(athenian_democratic_constitution__accountability_machinery, '4c6a9585-71d5-43f2-8e46-ae7d60873515').
narrative_ontology:cs_kernel_codification('4c6a9585-71d5-43f2-8e46-ae7d60873515', formalized).
narrative_ontology:cs_authority_grounding('4c6a9585-71d5-43f2-8e46-ae7d60873515', practice).
narrative_ontology:cs_interpretation_layer_present('4c6a9585-71d5-43f2-8e46-ae7d60873515').
narrative_ontology:cs_reading_relation('4c6a9585-71d5-43f2-8e46-ae7d60873515', athenian_democratic_constitution__assembly_supremacy, coexists_with).
narrative_ontology:cs_reading_relation('4c6a9585-71d5-43f2-8e46-ae7d60873515', athenian_democratic_constitution__exclusionary_base, coexists_with).
narrative_ontology:cs_reading_relation('4c6a9585-71d5-43f2-8e46-ae7d60873515', athenian_democratic_constitution__sortition_and_rotation, coexists_with).
narrative_ontology:cs_axiom('4c6a9585-71d5-43f2-8e46-ae7d60873515', foundational, office_holders_bear_personal_liability).
narrative_ontology:cs_axiom_status(office_holders_bear_personal_liability, holdable).
narrative_ontology:cs_axiom_grounding('4c6a9585-71d5-43f2-8e46-ae7d60873515', office_holders_bear_personal_liability, conventional).
narrative_ontology:cs_axiom('4c6a9585-71d5-43f2-8e46-ae7d60873515', foundational, demos_as_principal_constrains_agents).
narrative_ontology:cs_axiom_status(demos_as_principal_constrains_agents, holdable).
narrative_ontology:cs_axiom_grounding('4c6a9585-71d5-43f2-8e46-ae7d60873515', demos_as_principal_constrains_agents, deontological).
narrative_ontology:cs_reference_frame('4c6a9585-71d5-43f2-8e46-ae7d60873515', accountable_magistracy_framework).
narrative_ontology:cs_drift_state('4c6a9585-71d5-43f2-8e46-ae7d60873515', late_classical_to_hellenistic_transition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4c6a9585-71d5-43f2-8e46-ae7d60873515', '').
narrative_ontology:cs_kernel_id(athenian_democratic_constitution__accountability_machinery, athenian_democratic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(athenian_democratic_constitution__accountability_machinery, demos_as_principal).
narrative_ontology:constraint_victim(athenian_democratic_constitution__accountability_machinery, office_holders).
narrative_ontology:constraint_victim(athenian_democratic_constitution__accountability_machinery, legislative_proposers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROPOSER/OFFICE-HOLDER (SNARE) — Trapped within the accountability machinery. No exit from Athens; euthynai examination is mandatory upon leaving office. Personal wealth and freedom at stake (eisangelia can lead to seizure, exile, execution). High suppression: the threat of ostracism or judicial ruin prevents independent agency. Minimal coordination benefit — the office-holder may perceive duty to the demos, but this is internalized obligation, not genuine benefit-sharing. Maximum experienced extraction from this agent's position.
constraint_indexing:constraint_classification(athenian_democratic_constitution__accountability_machinery, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MAGISTRATE CLASS / ARISTOCRATIC FACTION (TANGLED ROPE) — Organized actors (the archons, generals, liturgists) experience both coordination and extraction. They benefit from the prestige and networks of office, and they participate in the auditing system that constrains other officials. But they also bear personal liability and face organized prosecution from political opponents (sycophants, rival factions). Constrained exit: they can refuse office-seeking, but doing so excludes them from power and prestige. The accountability machinery both protects property (through audits of others' conduct) and threatens it (through examination of their own).
constraint_indexing:constraint_classification(athenian_democratic_constitution__accountability_machinery, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: DEMOS AS PRINCIPAL (ROPE) — The accountability machinery coordinates the demos' control over executive agents. Euthynai, graphe paranomon, ostracism are mechanisms by which the assembled citizens monitor and constrain those who execute their will. From the demos' perspective, the auditing system is genuine coordination — it solves the collective action problem of monitoring agents on behalf of many. The demos benefits from suppressed unaccountable service; officials must explain themselves. This is pure coordination without significant extraction — the demos pays the cost of litigation and scrutiny, but receives the benefit of constraint. Arbitrage exit: the demos collectively defines what counts as accountability, can change the rules, and faces no external check.
constraint_indexing:constraint_classification(athenian_democratic_constitution__accountability_machinery, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: AMBITIOUS ORATOR/GENERAL (TANGLED ROPE) — Powerful individuals (Pericles, Themistocles, Demosthenes) experience the accountability machinery as both enabling and constraining. The system coordinates democratic deliberation — they gain influence through persuasion in the assembly. But it also extracts constant liability: one failed decree proposal (graphe paranomon) or unpopular war can result in prosecution, exile via ostracism, or confiscation. The 'whoever grew too large' clause is explicit extraction: exceptional power is punished as a threat to equality. Constrained exit: leaving office triggers examination; leaving Athens means exile. The ambitious agent is neither fully trapped nor fully mobile — they can moderate ambitions, but cannot escape scrutiny if they succeed.
constraint_indexing:constraint_classification(athenian_democratic_constitution__accountability_machinery, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: AUDIT RITUAL / INSTITUTIONAL FORM (PITON) — Over centuries, the euthynai and related audits became increasingly theatrical and formalistic. By the Hellenistic period, many examiners were elected rather than auditing with genuine scrutiny. The ritual persists because it embodies democratic legitimacy, but its functional capacity to constrain corrupt officials has degraded. Theater ratio: high (0.65+). The form of accountability (public examination, liability statements, logistai records) remains while the substance (genuine investigation, meaningful penalties) weakens. Maintained through institutional inertia — the demos continues to perform accountability even as the mechanism's teeth dull.
constraint_indexing:constraint_classification(athenian_democratic_constitution__accountability_machinery, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL CONSTRAINT (MOUNTAIN) — From a civilizational/universal view, some form of principal-agent monitoring is inherent to any delegation of authority: whenever one party governs on behalf of another, the principal must verify the agent's conduct. The accountability machinery appears as a structural necessity of governance itself, not as a contingent Athenian invention. However, this perspective risks naturalizing what is actually a specific institutional design choice. The schema will flag this as a false summit.
constraint_indexing:constraint_classification(athenian_democratic_constitution__accountability_machinery, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(athenian_democratic_constitution__accountability_machinery_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(athenian_democratic_constitution__accountability_machinery, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(athenian_democratic_constitution__accountability_machinery, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(athenian_democratic_constitution__accountability_machinery, TR),
    TR >= 0.70.

:- end_tests(athenian_democratic_constitution__accountability_machinery_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The accountability machinery does extract a cost from office-holders — mandatory examination, personal liability, risk of prosecution, threat of ostracism. However, the extraction is not maximal (snare-level 0.66+) because: (1) the beneficiary (demos) is also constrained by the same rules and cannot use the machinery arbitrarily without assembly consensus; (2) office-holding itself confers prestige, networks, and power that offset liability costs; (3) many office-holders exit examination without penalty. The moderate extraction reflects a genuine hybrid: the machinery coordinates the demos' principal-agent monitoring while also imposing real costs on agents. Suppression (0.52): Moderate-high. Officials cannot refuse examination (trapped into audit). Ostracism threat suppresses independent agency. Prosecution risk silences dissent. But suppression is not total because: (1) assembly protection limits arbitrary punishment; (2) sycophant-driven prosecution can be resisted through oratory; (3) officials retain agency to shape policy before examination. Theater ratio (0.35): Moderate-low in the early classical period, rising to 0.52 by late classical. Early accountability is functional — genuine investigation, meaningful penalties. By the Hellenistic period, examination becomes more performative; elected examiners (rather than auditors selected by lot) compromise independence. The rise in theater reflects piton degradation over time.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival diversity from a single structural set. The office-holder sees a snare — they are trapped in mandatory examination, bearing personal liability with no exit. The organized magistrate class sees tangled rope — they both benefit from prestige and suffer from liability, particularly when prosecuted by rivals. The demos sees rope — the accountability machinery is pure coordination of principal-agent monitoring. The ambitious orator sees tangled rope — they gain power through persuasion but lose it through examination and ostracism. The institutional audit system sees piton — the ritual persists though its substance weakens. The analytical observer risks seeing a mountain — accountability is inherent to governance — but the structural data (specific Athenian institutional design, beneficiaries, victims) reveals this as a false summit. The diversity of types from the same base properties demonstrates the framework's core claim: classification is not a property of the constraint alone, but of the constraint-relative-to-observer-position intersection.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural relationship to liability and scrutiny. Office-holders who bear personal risk and cannot exit (trapped + victim) experience maximum extraction: d ≈ 0.95, f(d) ≈ 1.42, maximum chi. Organized magistrates constrained by fellow officials and rival prosecution experience moderate extraction: d ≈ 0.55, f(d) ≈ 0.75. The demos as beneficiary with arbitrage exit (defining rules, facing no external constraint) experiences negative extraction: d ≈ 0.05, f(d) ≈ -0.12, enabling role. The ambitious powerful agent faces moderate extraction from ostracism threat: d ≈ 0.60, f(d) ≈ 0.85. Scope is local (Athens is a city-state); sigma modifier σ = 0.8 slightly dampens extracted chi at each perspective, but the perspectival ranking remains stable.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING EXEMPLAR: This constraint demonstrates how a single contested kernel (athenian_democratic_constitution) decomposes into structurally distinct readings. The accountability-machinery reading emphasizes audits, liability, and personal risk. The assembly-supremacy reading emphasizes the ekklesia's direct decision-making power. The exclusionary-base reading emphasizes the citizens' extraction from non-citizens. The sortition-and-rotation reading emphasizes lottery-filling as the anti-aristocratic principle. These are not disagreements about facts — they are different framings of what the constitution IS. The accountability-machinery reading answers: 'Athens' constitution is its audits' (examining officials, suppressing corruption, extracting liability from agents). This reading coexists with assembly-supremacy (the demos expresses its supremacy through audits) and influences (does not foreclose) sortition-and-rotation (lot-filling is compatible with accountability, though they interact in complex ways). The reading is challenged by the empirical drift (theater rising 0.25 → 0.52), suggesting that over time, the machinery's auditing function became increasingly theatrical. The false-summit analysis (perspective 6) applies here at a different level: the analytical observer might treat 'accountability is inherent to governance' as a universal principle, when in fact different constitutions solve the principal-agent problem differently (sortition vs. recall vs. term limits vs. audits). The Athenian solution is specific.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ostracism_as_constitutional_mechanism,
    'Is ostracism an integral constitutional limit on power concentration, or a fluid weapon for factional conflict?',
    'Historical pattern analysis: ostracism votes targeting specific individuals vs. abstract principle enforcement; examination of whether ostracized figures posed genuine threat or were political rivals',
    'If integral: accountability machinery is robust suppression mechanism (snare/tangled_rope classification stable). If fluid weapon: ostracism is capture-prone and theater rises (piton classification likely for later periods).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ostracism_as_constitutional_mechanism, empirical, 'Whether ostracism functions as constitutional limit or factional weapon').

omega_variable(
    euthynai_enforcement_capacity,
    'What proportion of office-holders faced meaningful penalty through euthynai examination vs. pro forma clearance?',
    'Systematic review of logistai records, Athenian law court documents, and forensic speeches; quantification of indictments, convictions, and penalty magnitudes per decade',
    'If meaningful penalty rate > 15%: suppression is real structural feature. If < 5%: suppression is largely theatrical, extractiveness lower than claimed (0.38), theater ratio higher than claimed (0.35).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(euthynai_enforcement_capacity, empirical, 'Actual enforcement rate of euthynai audits').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the accountability-machinery reading foreclose the assembly-supremacy reading, or do both coexist as compatible framings of the same institution?',
    'Textual analysis of constitutional sources (Aristotle''s Constitution of the Athenians, Plutarch, inscriptions); assessment of whether accountability mechanisms are presented as constraints on assembly power or expressions of it',
    'If foreclose: these are alternative constitutional theories in genuine logical conflict. If coexist: both are legitimate readings of the same structure — assembly is supreme, AND it expresses its supremacy through accountability. This affects cs_structure.reading_relations typing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Logical relationship between accountability-machinery and assembly-supremacy readings').

omega_variable(
    alien_and_slave_extraction_relation,
    'How does this accountability machinery relate to the exclusionary-base reading? Does accountability of citizens depend on prior exclusion of non-citizens?',
    'Structural analysis: whether the accountability costs paid by office-holders are affordable only because political authority extracts from non-citizen groups; whether excluding metics and slaves from office removes them from accountability risk (making citizenship''s burdens appear voluntary rather than coerced)',
    'If dependent: the accountability machinery is structurally intertwined with exclusion — cannot be understood in isolation. Beneficiary shifts from ''demos'' to ''citizen demos specifically via non-citizen extraction.'' If independent: accountability is a distinct mechanism that happens to operate within a prior exclusionary frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alien_and_slave_extraction_relation, conceptual, 'Structural relation between accountability machinery and exclusionary base').

omega_variable(
    sortition_interaction_with_accountability,
    'Does the sortition-and-rotation reading (offices filled by lot, rotated yearly) strengthen or weaken the accountability machinery''s effectiveness?',
    'Comparative analysis: accountability constraints on elected officials vs. lot-drawn officials; examination of whether rotation prevents accumulation of power (supporting piton de-escalation) or undermines institutional memory needed for effective audits',
    'If strengthens: sortition and accountability are complementary (both reduce power concentration); readings coexist naturally. If weakens: sortition undermines audit effectiveness because officials lack expertise; readings are in tension (influence relation vs. coexists relation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sortition_interaction_with_accountability, empirical, 'Whether sortition strengthens or weakens accountability mechanism').

omega_variable(
    literacy_and_audit_accessibility,
    'How much did limited literacy among the demos affect their capacity to scrutinize written audit records (logistai accounts)?',
    'Historical demographics of literacy in classical Athens; analysis of how audit records were presented (oral summaries vs. written documents); examination of whether illiteracy created information asymmetry that enabled corruption',
    'If high impact: suppression of corrupt officials is weaker than claimed because demos cannot verify the records themselves; extractiveness rises. If low impact: oral presentation and trained examiners compensated; mechanism effective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literacy_and_audit_accessibility, empirical, 'Effect of limited literacy on audit mechanism effectiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(athenian_democratic_constitution__accountability_machinery, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ath_acc_theater_early_classical, athenian_democratic_constitution__accountability_machinery, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ath_acc_theater_mid_classical, athenian_democratic_constitution__accountability_machinery, theater_ratio, 50, 0.35).
narrative_ontology:measurement(ath_acc_theater_late_classical, athenian_democratic_constitution__accountability_machinery, theater_ratio, 100, 0.52).

% Extraction over time
narrative_ontology:measurement(ath_acc_extractiveness_early_classical, athenian_democratic_constitution__accountability_machinery, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ath_acc_extractiveness_mid_classical, athenian_democratic_constitution__accountability_machinery, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(ath_acc_extractiveness_late_classical, athenian_democratic_constitution__accountability_machinery, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ath_acc_suppression_early_classical, athenian_democratic_constitution__accountability_machinery, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(ath_acc_suppression_mid_classical, athenian_democratic_constitution__accountability_machinery, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(ath_acc_suppression_late_classical, athenian_democratic_constitution__accountability_machinery, suppression_requirement, 100, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(athenian_democratic_constitution__accountability_machinery, enforcement_mechanism).
narrative_ontology:affects_constraint(athenian_democratic_constitution__accountability_machinery, athenian_democratic_constitution__assembly_supremacy).
narrative_ontology:affects_constraint(athenian_democratic_constitution__accountability_machinery, athenian_democratic_constitution__exclusionary_base).
narrative_ontology:affects_constraint(athenian_democratic_constitution__accountability_machinery, athenian_democratic_constitution__sortition_and_rotation).

% DUAL FORMULATION NOTE:
% The athenian_democratic_constitution kernel decomposes into four structurally distinct readings, each with its own extractiveness value, beneficiary/victim structure, and mechanisms. The accountability-machinery reading (this story) has ε=0.38 and emphasizes audits, liability, and personal risk extraction from officials. Assembly-supremacy emphasizes direct decision-making power; exclusionary-base emphasizes extraction from non-citizens enabling citizen political participation; sortition-and-rotation emphasizes lottery and annual rotation as anti-aristocratic principles. Each reading is a coherent constraint story with its own perspectives, measurements, and omega variables. They link through the kernel: all are interpretations of what the Athenian constitutional structure IS.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
