% ============================================================================
% CONSTRAINT STORY: trial_of_socrates_low_bandwidth_inner
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trial_of_socrates_low_bandwidth_inner, []).

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
 *   constraint_id: trial_of_socrates_low_bandwidth_inner
 *   human_readable: Trial of Socrates: Outer Prosecution of Low-Bandwidth Inner Philosophical Practice
 *   domain: ancient_politics/philosophical_persecution
 *
 * SUMMARY:
 *   In 399 BCE, Athens prosecuted Socrates on charges of impiety and
 *   corruption of youth. The trial instantiates a structural constraint
 *   between nested institutional containers with mismatched bandwidth. The
 *   Socratic practice — elenchus, examination of received assumptions about
 *   virtue and knowledge, systematic revision of inherited beliefs — operated
 *   with high informal bandwidth (active intellectual engagement, continuous
 *   reframing of foundations) but zero formal institutional bandwidth (no
 *   recognized legal procedure for philosophical amendment of sacred
 *   tradition, no legitimate category for systematic questioning of piety).
 *   The polis's outer container (religious law, assembly procedure, jury
 *   decision mechanism) had high formal bandwidth for boundary enforcement
 *   (well-developed impiety statutes, established prosecution procedures) but
 *   had to apply these categories to intellectual drift they were not
 *   designed to process. The constraint emerges where outer-container
 *   procedure meets inner-container drift: the polis could suppress Socratic
 *   inquiry using inherited legal categories, but could not name what it was
 *   suppressing without admitting that philosophical practice operates as a
 *   legitimate domain with its own kernel revision function. The prosecution
 *   and execution of Socrates are both the constraint's enforcement mechanism
 *   and evidence of its existence — the polis demonstrates that it cannot
 *   process Socratic practice legitimately, only suppress it as violation of
 *   existing categories.
 *
 * KEY AGENTS:
 *   - Socrates: Primary victim (powerless/trapped) — prosecuted under charges that cannot name his actual practice; faces death or exile with no legitimate procedural defense
 *   - Prosecutorial Coalition (Anytus, Meletus, Lycon): Primary beneficiary (institutional/arbitrage) — organized to restore religious-civic order and reaffirm ancestral piety boundaries; experiences prosecution as legitimate coordination
 *   - Athenian Democratic Assembly: Secondary institutional actor (organized/constrained) — the jury vote coordinates polis continuity while extracting Socrates as scapegoat; lacks legitimate categories to process philosophical drift except as impiety
 *   - Athenian Religious-Civic Structure: Institutional beneficiary (institutional/arbitrage) — maintains boundary enforcement against philosophical relativism through inherited ritual categories; experiences constraint as necessary maintenance despite rising theater
 *   - Post-Socratic Philosophical Tradition: Long-term actor (powerful/mobile) — Socrates's death paradoxically demonstrates the unsustainability of prosecuting philosophy; subsequent Academy and Lyceum formalize philosophical practice as legitimate institutional domain with its own procedures
 *   - Analytical Observer at Civilizational Scale: External perspective (analytical/analytical) — observes nested-container incompatibility as structural inevitability when outer formal bandwidth meets inner low formal bandwidth
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trial_of_socrates_low_bandwidth_inner, 0.68).
domain_priors:suppression_score(trial_of_socrates_low_bandwidth_inner, 0.75).
domain_priors:theater_ratio(trial_of_socrates_low_bandwidth_inner, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trial_of_socrates_low_bandwidth_inner, extractiveness, 0.68).
narrative_ontology:constraint_metric(trial_of_socrates_low_bandwidth_inner, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(trial_of_socrates_low_bandwidth_inner, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trial_of_socrates_low_bandwidth_inner, snare).
narrative_ontology:human_readable(trial_of_socrates_low_bandwidth_inner, "Trial of Socrates: Outer Prosecution of Low-Bandwidth Inner Philosophical Practice").
narrative_ontology:topic_domain(trial_of_socrates_low_bandwidth_inner, "ancient_politics/philosophical_persecution").

domain_priors:requires_active_enforcement(trial_of_socrates_low_bandwidth_inner).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trial_of_socrates_low_bandwidth_inner, athenian_religious_civic_order).
narrative_ontology:constraint_beneficiary(trial_of_socrates_low_bandwidth_inner, prosecutorial_faction).
narrative_ontology:constraint_victim(trial_of_socrates_low_bandwidth_inner, socratic_philosophical_practice).
narrative_ontology:constraint_victim(trial_of_socrates_low_bandwidth_inner, socrates_individual).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SOCRATES (SNARE) — Trapped without legitimate procedural defense. His actual practice (Socratic inquiry revising assumptions about virtue, knowledge, piety) has no formal category within the polis's structures. Prosecuted under charges of impiety and youth corruption — categories that exist precisely because philosophical practice lacks a recognized amendment channel. Maximum extraction: face death or exile with no legitimate institutional recourse. The constraint exists to suppress the operation that cannot be named.
constraint_indexing:constraint_classification(trial_of_socrates_low_bandwidth_inner, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PROSECUTORIAL COALITION (ROPE) — Organized to coordinate restoration of polis religious-civic order threatened by uncontrolled philosophical inquiry. From this perspective, the trial is a legitimate coordination mechanism: it re-establishes boundaries, reaffirms collective agreement on piety and education, maintains ancestral religion against intellectual erosion. The prosecution experiences the constraint as pure coordination with no extraction — they are solving a genuine collective action problem (maintaining shared sacred order). Beneficiaries with institutional power and arbitrage options.
constraint_indexing:constraint_classification(trial_of_socrates_low_bandwidth_inner, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: ATHENIAN DEMOCRATIC ASSEMBLY (TANGLED_ROPE) — The democratic apparatus coordinates continuity of the polis (genuine coordination function) while extracting Socrates as scapegoat for accumulated decades of unease about philosophical relativism. The assembly faces constrained exit: cannot simply ignore the philosophical drift (accumulation of sophistic and Socratic inquiry has genuinely destabilized inherited assumptions), but lacks legitimate categories to process this drift except as impiety. The assembly's own decision procedure — the jury vote — is both coordinating mechanism and extraction apparatus. Active enforcement required: the trial consumes assembly resources, requires legal procedure, requires civic participation.
constraint_indexing:constraint_classification(trial_of_socrates_low_bandwidth_inner, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: RELIGIOUS-CIVIC RITUAL STRUCTURE (PITON) — From a generational view, the boundary enforcement against philosophy is increasingly theatrical. The charges of impiety and youth corruption are the available categories, but their performative content rises as they become the only legitimate way to suppress intellectual drift. The ritual persists (the trial must occur, the votes must happen) but the true function — suppressing low-bandwidth philosophical change — cannot be named without revealing the mechanism. Theater ratio high because the trial enacts inherited categories (impiety law, youth corruption statute) that no longer fully describe the actual threat.
constraint_indexing:constraint_classification(trial_of_socrates_low_bandwidth_inner, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NESTED CONTAINER VIEW (MOUNTAIN) — From civilizational distance, the trial instantiates a structural necessity: when outer containers (polis, assembly, law) have high formal bandwidth and inner containers (philosophical practice) operate with low formal bandwidth, friction at the seam is inevitable. The outer process must apply its existing categories to inner drift — and if those categories are mismatched (impiety law applied to epistemological method), the outcome appears as law executing its function on anomalous activity. The mountain classification reflects that this nested-container incompatibility is a structural inevitability, not a contingent Athenian policy choice.
constraint_indexing:constraint_classification(trial_of_socrates_low_bandwidth_inner, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: POST-SOCRATIC PHILOSOPHICAL TRADITION (SCAFFOLD) — Paradoxically, Socrates's death creates a sunset structure for the constraint. The trial demonstrates the unsustainability of prosecuting philosophy as impiety: Plato's Academy, Aristotle's Lyceum, and the systematic philosophical schools that follow Socrates's death show that constraint on philosophical practice cannot be maintained. The prosecution succeeds (Socrates dies) but fails (philosophy institutionalizes and becomes legitimate). From the perspective of emerging philosophical institutions, the trial is a temporary bottleneck with a structural sunset: philosophy's bandwidth increases until it gains formal recognition within the polis as a legitimate practice domain. High power and mobile exit options because philosophy as a tradition can relocate (geographically), evolve (methodologically), and gain institutional recognition (academy, school).
constraint_indexing:constraint_classification(trial_of_socrates_low_bandwidth_inner, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trial_of_socrates_low_bandwidth_inner_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trial_of_socrates_low_bandwidth_inner, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trial_of_socrates_low_bandwidth_inner, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trial_of_socrates_low_bandwidth_inner, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trial_of_socrates_low_bandwidth_inner, TR),
    TR >= 0.70.

:- end_tests(trial_of_socrates_low_bandwidth_inner_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. Early sophistic era (time 0) shows moderate extraction (0.42) — intellectual inquiry is increasing but not yet systematically prosecuted. Mid-Socratic practice (time 25) shows rising extractiveness (0.55) — decades of accumulated philosophical inquiry create collective unease and accumulating pressure to suppress. Trial and execution (time 40) show maximum extractiveness (0.68) — death penalty extracts the maximum possible cost from Socratic practice and its practitioners. The rising trajectory reflects constraint accumulation: as philosophical practice becomes more systematized and influential, the polis's response intensifies despite the practice having no formal illegality. Suppression (0.75): Very high. Suppression mechanisms include: (1) formal barriers (impiety law applicable to any intellectual, no formal category for philosophical inquiry), (2) institutional barriers (no academy or school structure within the polis to legitimize philosophy), (3) existential threat (death penalty for practitioners), (4) ideological barriers (polis identity constitutively tied to inherited religious-civic order). Theater ratio (0.68): High and rising. Early sophistic era shows lower theater (0.45) — intellectual relativism is novel and generates genuine anxiety. By trial time (0.68), the constraint operates substantially through inherited procedural theater: the trial enacts formal categories (impiety law, youth corruption statute) that increasingly fail to capture the actual mechanism (suppression of low-bandwidth-inner practice using high-bandwidth-outer categories). The rising theater reflects that the same enforcement categories must bear growing weight as the underlying drift expands. The trial itself is highly theatrical: the jury votes on impiety, but no one (including prosecutors) claims Socrates actually disbelieves in gods — the theater is the only available procedure for suppressing philosophical practice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence over what is being enforced and why. Socrates sees prosecution without legitimate defense category — his actual practice (elenchus, questioning assumptions) has no formal name within polis law, so he cannot defend himself except by denial of charges that misname his activity. The prosecution sees coordination mechanism — they are restoring boundaries, reaffirming collective religious-civic order against intellectual erosion. The assembly sees mixed coordination and extraction — they maintain polis continuity (genuine coordination) while eliminating a threat through law (extraction), using categories that no longer match the actual practice. The religious-civic structure sees maintenance of boundary and order through inherited ritual. The analytical observer at civilizational scale sees nested-container incompatibility — an inherent structural tension between outer formal bandwidth (law, procedure) and inner low formal bandwidth (philosophical practice) that generates friction where the two meet. These are not different readings of the same phenomenon — they are genuinely incommensurable perspectives on what the constraint is and how it operates.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values reflect structural extraction flow. Socrates as trapped victim bearing maximum cost: d ≈ 0.95 (victim + trapped exit + powerless + biographical horizon = high f(d) ≈ 1.42). Prosecutorial coalition as beneficiaries with arbitrage options: d ≈ 0.05 (beneficiary + arbitrage exit + institutional power = low f(d) ≈ -0.12). Athenian assembly as constrained actors with mixed benefits and costs: d ≈ 0.55 (mixed beneficiary-victim role + constrained exit + organized power = moderate f(d) ≈ 0.75). These directionality values drive the effective extractiveness χ = ε × f(d) × σ(S): Socrates experiences χ ≈ 0.68 × 1.42 × 0.8 ≈ 0.77 (snare territory). Prosecution experiences χ ≈ 0.68 × (-0.12) × 0.8 ≈ -0.065 (negative — they experience benefit, not extraction). Assembly experiences χ ≈ 0.68 × 0.75 × 0.8 ≈ 0.41 (tangled rope territory — significant extraction but mixed with coordination function). The perspectival gap in χ values (negative for prosecution, high positive for Socrates, moderate for assembly) reveals the extraction distribution: concentrated on low-power victims with no exit, experienced as non-extraction by beneficiaries, experienced as mixed burden-and-benefit by coordinating institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint demonstrates the mandatrophy by showing that all readings are locally legitimate given the observer's structural position, but globally the constraint cannot be classified as single type. Socrates legitimately sees a Snare (high extraction, maximum suppression, no procedural defense). Prosecution legitimately sees a Rope (pure coordination mechanism solving collective action problem of polis order). Assembly legitimately sees a Tangled Rope (coordination with extraction). The religious-civic structure sees maintenance ritual (Piton). The philosophical tradition post-Socrates sees a temporary constraint with sunset (Scaffold — prosecution ultimately fails to suppress philosophy). The civilizational observer sees structural inevitability (Mountain — nested container incompatibility). The mandatrophy resolves not by choosing a single 'correct' classification but by recognizing that the constraint's essential property IS the perspectival divergence: the polis cannot create a shared category that would make the constraint's function transparent to all observers. This opacity is the mechanism of the snare — Socrates cannot defend himself because the defense (legitimate philosophical practice) has no institutional category. The constraint exists precisely because the categories are incommensurable. The prosecution wins the trial and loses history — post-Socratic philosophy institutionalizes, proving that the constraint could not be sustained against the structural forces it attempted to suppress. The sunset of this constraint comes when philosophical practice gains formal legitimacy (Plato's Academy, Aristotle's Lyceum, eventually integration into polis education), creating new institutional containers with higher formal bandwidth for philosophical amendment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    impiety_charge_actual_referent,
    'Do the charges of impiety and corruption of youth accurately name Socrates''s actual practice, or are they institutional categories applied because legitimate categories for philosophical practice do not exist?',
    'Textual analysis of Plato''s Apology and Euthyphro comparing Socrates''s actual methods (elenchus, examination of knowledge-claims) against the legal definitions of impiety (non-belief in gods, introducing new gods) and youth corruption (encouraging disobedience to elders, destabilizing traditional authority). If the trial record shows the prosecution struggling to map actual practice onto legal categories, the charge is institutional mislabeling rather than accurate naming.',
    'If charges accurately name Socrates''s practice: the trial is legitimate boundary enforcement (reduces constraint severity to Rope or Scaffold). If charges are institutional mislabeling: the trial is persecution of a practice for which no legitimate procedural defense exists (confirms Snare classification, suggests low-bandwidth-inner structure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(impiety_charge_actual_referent, empirical, 'Whether impiety charges accurately name or institutionally mislabel philosophical practice').

omega_variable(
    philosophical_drift_accumulation_timeline,
    'Did accumulation of sophistic and Socratic inquiry over preceding decades destabilize enough of the polis''s inherited assumptions to generate genuine collective unease, or was the prosecution a factional power grab by a conservative bloc?',
    'Historical analysis of late 5th-century Athens: sophistic education expansion (Protagoras, Gorgias); intellectual relativism in Athenian elite; shift in assumptions about gods (euhemerism, natural philosophy); correlation between philosophical school prominence and prosecution timing. If unease is measurable across multiple social strata before 399, drift-accumulation model holds. If prosecution follows factional conflict (Thirty Tyrants oligarchy, post-war political realignment), power-grab model holds.',
    'If accumulation: polis truly faced bandwidth crisis (Tangled Rope and Scaffold models apply; constraint is at least partially necessary). If factional grab: prosecution is pure extraction using inherited categories as pretext (Snare classification confirmed; constraint is opportunistic, not structural).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(philosophical_drift_accumulation_timeline, empirical, 'Whether philosophical drift created genuine collective unease or prosecution was opportunistic factional conflict').

omega_variable(
    alternative_amendment_channel_availability,
    'Could the Athenian polis have formalized philosophical practice as a legitimate domain with its own kernel revision procedure — or was the constraint (outer suppression of inner low-bandwidth practice) inevitable given the polis''s political-religious structure?',
    'Counterfactual analysis: What institutional changes would have been required to create legitimate space for Socratic inquiry within the polis (formal recognition of philosophy schools, separate statutes for intellectual vs religious domains, amendment procedures for inherited assumptions)? Would these changes have been compatible with Athenian democratic theocracy or would they have required fundamental restructuring of polis identity?',
    'If alternative channel feasible: the constraint is contingent (polis chose suppression over formalization). Classification shifts toward Snare (oppression of practice with alternatives available). If alternative requires fundamental restructuring: the constraint is structural to the polis form (remains Mountain at civilizational scale; Snare at biographical scale is still accurate but appears as tragic necessity rather than opportunistic choice).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_amendment_channel_availability, conceptual, 'Whether the polis could have legitimized philosophical practice or suppression was structurally inevitable').

omega_variable(
    spectral_guilt_of_jury,
    'Did the jury voting to execute Socrates genuinely believe in the charges, or did they vote to maintain social order despite epistemic doubt about his actual guilt?',
    'Textual analysis of jury composition, voting patterns, and post-trial commentary. Did jury members later express doubt or regret? Did Athenian intellectuals (Xenophon, Plato, Aristophanes''s surviving works) characterize the vote as just enforcement or as tragic necessity? If significant epistemic doubt exists in historical record despite majority voting guilty, suppression mechanism was social-order maintenance rather than shared judgment of culpability.',
    'If genuine belief in charges: jury verdict reflects coordinated application of shared values (reduces extraction component, supports Rope/Scaffold readings). If epistemic doubt with social-order maintenance: jury enabled the snare despite doubts (extraction component high, confirms Snare classification; suppression mechanism includes cognitive/social forcing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spectral_guilt_of_jury, empirical, 'Whether jury vote expressed genuine belief in charges or doubt masked by social-order maintenance').

omega_variable(
    low_bandwidth_inner_mechanism,
    'Is the constraint''s core mechanism the formal institutional inability to process philosophical practice as a legitimate amendment channel, or is it ideological (the polis could not conceptually accept that inherited assumptions might require revision)?',
    'Distinguish between formal bandwidth (structural categories available in law and procedure) and conceptual bandwidth (polis''s willingness to revise inherited worldview). Did Athenians lack the formal procedure to legitimize philosophy, or did they reject the concept that philosophy legitimately revised sacred tradition? Post-Socratic evidence: did philosophy gain acceptance through procedural formalization (academy charter) or through gradual ideological shift (philosophical schools became prestigious despite lack of formal authorization)?',
    'If formal bandwidth the constraint: future polities can solve by creating legitimate amendment procedures for philosophical practice (Scaffold with clear sunset as procedural formalization spreads). If conceptual bandwidth the constraint: ideological resistance to revision-of-tradition persists even with formal procedures available (constraint is deeper, more resistant to procedural solutions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(low_bandwidth_inner_mechanism, conceptual, 'Whether constraint is formal-bandwidth institutional mismatch or conceptual rejection of tradition-revision').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trial_of_socrates_low_bandwidth_inner, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_early_sophistic, trial_of_socrates_low_bandwidth_inner, theater_ratio, 0, 0.45).
narrative_ontology:measurement(theater_mid_socratic, trial_of_socrates_low_bandwidth_inner, theater_ratio, 25, 0.58).
narrative_ontology:measurement(theater_trial, trial_of_socrates_low_bandwidth_inner, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(extract_early_sophistic_era, trial_of_socrates_low_bandwidth_inner, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(extract_mid_socratic_practice, trial_of_socrates_low_bandwidth_inner, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(extract_trial_execution, trial_of_socrates_low_bandwidth_inner, base_extractiveness, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trial_of_socrates_low_bandwidth_inner, identity_coordination).
narrative_ontology:affects_constraint(trial_of_socrates_low_bandwidth_inner, athenian_sophistic_displacement).
narrative_ontology:affects_constraint(trial_of_socrates_low_bandwidth_inner, post_socratic_academy_legitimation).

% DUAL FORMULATION NOTE:
% The trial of Socrates represents a constraint at the intersection of two distinct domains: (1) suppression of low-bandwidth-inner practice using high-bandwidth-outer categories (nested container incompatibility), and (2) ideological resistance to philosophical revision of inherited religious-civic identity. These could be formalized as separate stories with different ε values: formal-bandwidth constraint (ε≈0.55, emphasizing institutional procedure) vs. conceptual-bandwidth constraint (ε≈0.72, emphasizing identity resistance). The unified story treats the constraint as simultaneous institutional and ideological — they are inseparable in the trial mechanism. The upstream constraint (sophistic displacement from paideia) created conditions for the trial; the downstream constraint (post-Socratic Academy legitimation) shows the sunset of prosecution-based suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trial_of_socrates_low_bandwidth_inner, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
