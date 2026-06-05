% ============================================================================
% CONSTRAINT STORY: birth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_birth_reading, []).

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
 *   constraint_id: birth_reading
 *   human_readable: Moral Status Begins at Birth (Birth-Reading of Personhood Boundary)
 *   domain: bioethics/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   The birth reading of the personhood boundary is an institutional
 *   assertion that legal and moral personhood begins at birth, not at
 *   conception or viability. Under this reading, the fetus is a potential
 *   person but not a rights-bearing subject; abortion is a healthcare
 *   decision within the pregnant person's autonomy; state enforcement of
 *   fetal interests is illegitimate; and no separate legal standing exists
 *   for fetal advocates to constrain abortion access. This reading generates
 *   a tangled hybrid constraint: it coordinates reproductive autonomy
 *   (genuine coordination function for pregnant persons and abortion access
 *   infrastructure) while simultaneously extracting from fetal advocates and
 *   conservative religious communities by foreclosing their metaphysical
 *   claims about fetal personhood within legal/medical contexts. The
 *   constraint's theater ratio (0.55) reflects a growing performative gap:
 *   institutional commitment to the birth reading coexists with fragmentary
 *   enforcement (legal abortion bans in some jurisdictions, protected access
 *   in others), medical cognitive dissonance (doctors trained under the
 *   reading but practicing under contradictory state mandates), and
 *   international institutional fracturing (Global North vs. Global South
 *   positions on fetal personhood). The extractiveness (0.58) has risen over
 *   30 years as conservative political mobilization has intensified pressure
 *   on the reading's institutional dominance — the reading now requires more
 *   active enforcement and faces greater suppression of its authority claims.
 *
 * KEY AGENTS:
 *   - Pregnant persons seeking abortion: Primary beneficiary (moderate/constrained) — reading empowers autonomy but constrains access via enforcement variation
 *   - Abortion access providers: Primary beneficiary (institutional/arbitrage) — reading legitimizes their professional role; they could adopt alternative readings but benefit from this one
 *   - Fetal advocate movements: Primary victim (powerless/trapped) — reading forecloses their core claim (separate fetal personhood); no exit from this epistemic frame
 *   - Conservative religious institutions: Secondary victim (institutional/constrained) — reading delegitimizes their personhood doctrine within secular law; retain political power but lose metaphysical standing
 *   - Medical licensing bodies: Institutional actor (institutional/arbitrage) — maintain the reading as normative doctrine while clinical practice fragments across geopolitical boundaries
 *   - International human rights framework: Organized actor (organized/constrained) — institutionalizes the reading but faces generational backlash from Global South religious actors
 *   - Analytical observer: Civilizational position (analytical/analytical) — risks naturalizing a contingent institutional reading as a law of political organization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(birth_reading, 0.58).
domain_priors:suppression_score(birth_reading, 0.68).
domain_priors:theater_ratio(birth_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(birth_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(birth_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(birth_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(birth_reading, tangled_rope).
narrative_ontology:human_readable(birth_reading, "Moral Status Begins at Birth (Birth-Reading of Personhood Boundary)").
narrative_ontology:topic_domain(birth_reading, "bioethics/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(birth_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(birth_reading, pregnant_persons).
narrative_ontology:constraint_beneficiary(birth_reading, abortion_access_providers).
narrative_ontology:constraint_victim(birth_reading, fetal_advocates).
narrative_ontology:constraint_victim(birth_reading, conservative_religious_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FETAL ADVOCATE MOVEMENT (SNARE) — Under this reading, fetal advocates are structurally excluded from the personhood claim they seek to defend. The reading itself forecloses their primary demand: legal personhood for the fetus. They are trapped without exit: they cannot persuade courts or legislatures to recognize fetal interests as separate from the pregnant person's autonomy under this framework. The constraint extracts from them by denying standing to advocate for entities they believe are moral patients. No coordination function exists for this agent — the birth reading provides zero accommodation to fetal-interest advocates within its epistemic frame.
constraint_indexing:constraint_classification(birth_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ABORTION RIGHTS LEGAL INFRASTRUCTURE (ROPE) — Institutional actors (Planned Parenthood, NARAL, abortion access organizations, sympathetic legal scholars) experience this constraint as pure coordination: the birth reading provides a clear principled foundation for defending abortion access and opposing state-mandated fetal protection laws. Their political and legal positions are entirely aligned with this framing. They benefit from the clarity and institutional support the reading provides. This is coordination, not mixed extraction — these agents have strong arbitrage exit (they could adopt alternative readings) but actively choose this one because it serves their structural interests cleanly.
constraint_indexing:constraint_classification(birth_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PREGNANT PERSONS SEEKING ABORTION (TANGLED ROPE) — This reading frames abortion as a healthcare decision entirely within the pregnant person's autonomy (coordination function: clarifies decision-making authority). But the constraint operates through legal and medical enforcement: access depends on regulatory geography, provider availability, financial barriers, and social stigma. Pregnant persons bear the full biological and social cost of pregnancy continuation if the reading's legal protection fails (suppression via geopolitical fragmentation and enforcement variation). They experience genuine coordination at the normative level (the reading empowers them) and genuine extraction at the enforcement level (legal bans, clinic closures, provider harassment). This is tangled — cannot be disaggregated into pure coordination or pure extraction.
constraint_indexing:constraint_classification(birth_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL HUMAN RIGHTS FRAMEWORK (SCAFFOLD) — From a generational/continental perspective, this reading instantiates a temporary coordination mechanism aligned with reproductive autonomy norms currently dominant in Western human rights discourse (CEDAW, European human rights law). However, this alignment itself has an implicit sunset: as global demographics shift, conservative religious actors (particularly from the Global South) are successfully reframing the personhood boundary toward conception in international forums (UN declarations, regional religious coalitions). The scaffold classification reflects that this reading's institutional dominance is contingent on a political majority that is narrowing. The sunset is endogenous: the reading's success at suppressing fetal-advocate political voice is creating backlash pressure that will shift the institutional consensus toward alternative readings within 20-40 years.
constraint_indexing:constraint_classification(birth_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: MEDICAL LICENSING AND PROFESSIONAL NORMS (PITON) — Medical organizations (ACOG, WHO) have adopted the birth reading as institutional doctrine, but the actual clinical practice is substantially degraded theater: doctors in restrictive jurisdictions must navigate cognitive dissonance between the reading's autonomy principle and state-mandated waiting periods, parental consent laws, gestational age restrictions, and fetal viability doctrine (which contradicts the birth reading's categorical claim). The professional norm persists through institutional inertia — medical associations maintain the reading as official position while clinical reality fractures across geopolitical boundaries. The theater ratio (0.55) reflects this gap: the normative commitment to the birth reading is genuine, but its enforcement is partial and compromised, creating performative compliance rather than coherent practice.
constraint_indexing:constraint_classification(birth_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSERVATIVE RELIGIOUS INSTITUTIONAL AUTHORITY (TANGLED ROPE) — This perspective experiences the birth reading as both constraining and partially extractive. The constraint coordinates religious communities around a clear enemy (secular abortion law) and mobilizes institutional resources (churches, schools, charitable networks) toward political opposition. But the reading also extracts from religious authority by denying institutional standing to claims religious leaders make about fetal moral status — their epistemic position is delegitimized within secular legal and medical contexts. Religious institutions are trapped between organizational benefits (mobilization around shared opposition) and institutional losses (marginalizing their metaphysical claims about personhood). They are constrained but not trapped — they retain political power and exit options (litigation, legislative capture at state level), but their fundamental authority claim (personhood doctrine) is foreclosed within this reading.
constraint_indexing:constraint_classification(birth_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal analytical perspective, one could argue the birth reading instantiates a fundamental metaphysical fact: legal personhood is a social construct whose boundaries are determined by political/legal communities, and birth is a natural discontinuity that provides a clear demarcation (live birth = exit from dependence on another's bodily resources). This perspective naturalizes the reading as a law of political organization. However, the structural data contradicts this: the reading has identifiable beneficiaries, active enforcement requirements, suppression mechanisms, and a narrow geopolitical distribution. The analytical observer's mountain classification will trigger false summit detection — revealing that 'birth as natural demarcation' is not a law of nature but a contingent institutional reading that benefits specific agents.
constraint_indexing:constraint_classification(birth_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(birth_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(birth_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(birth_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(birth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(birth_reading, TR),
    TR >= 0.70.

:- end_tests(birth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The birth reading concentrates decision authority in pregnant persons and abortion-access infrastructure while structurally excluding fetal advocate voices from policy debates. This is genuine extraction from the fetal-advocate perspective — they are denied standing and political power to defend their core claim. The reading achieves this through epistemic foreclosure rather than resource capture: it rewrites the boundary of who counts as a person, thereby removing the fetus from the moral patient set. Over 30 years (0 to 30), extractiveness has risen from 0.48 to 0.58 because conservative religious mobilization (particularly post-Dobbs in the US) has forced the birth reading to defend itself more actively and explicitly. Earlier, the reading operated with less friction and lower theater. Suppression (0.68): High. Multiple barriers prevent exit or alternative framings: legal prohibition of abortion (in bans), provider harassment and clinic closures, geographic access barriers, financial obstacles, social stigma, medical conscience clause complications, and institutional delegitimization of fetal-advocate voices. These are both structural (legal/economic) and partly internalized (identity-based acceptance of abortion stigma). Pregnant persons in restrictive jurisdictions face trapped-level suppression; in permissive jurisdictions, constrained-level suppression. Theater ratio (0.55): Moderate-high. The gap between the reading's normative clarity (birth is a natural boundary) and enforcement reality (fragmentary, contradictory across geopolitics, degraded by medical conscience clauses) has widened. Medical professionals maintain the birth reading as institutional doctrine while practicing under state-mandated waiting periods and viability criteria that contradict the reading's categorical claims. International human rights bodies affirm the reading while Global South resistance strengthens. The reading requires more performative work to maintain coherence as enforcement fragments.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits dramatic perspectival divergence across institutional and power positions. Abortion access infrastructure sees pure coordination (Rope) — the reading gives them clear institutional authority. Pregnant persons see mixed coordination-extraction (Tangled Rope) — the reading empowers them normatively but their actual access is suppressed by enforcement variation. Fetal advocates see extraction only (Snare) — the reading structurally forecloses their position with no coordination benefit. Conservative religious institutions see partial extraction mixed with mobilization benefit (Tangled Rope) — they lose metaphysical standing but gain organizational coherence through opposition. International human rights frameworks see a temporary institutional norm (Scaffold) — currently dominant but facing generational backlash. Medical professionals see incoherent performance (Piton) — trained under the reading but forced to practice under contradictory mandates. The analytical observer risks seeing an inevitable natural law (Mountain) — birth as a self-evident boundary — but the structural data reveals this as a false summit. The perspectival gaps are not measurement artifacts; they reflect real differences in how the reading distributes authority, suppression, and extraction across agents with different institutional positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The birth reading's directionality operates by redefining the personhood boundary and thereby determining which agents count as subjects of rights. Pregnant persons and abortion-access infrastructure are beneficiaries (d ≈ 0.10-0.15) with low extracted costs because the reading places them at the center of decision authority. Fetal advocates are victims (d ≈ 0.90) with maximum extraction because the reading explicitly excludes the entity they advocate for from the moral patient set. Conservative religious institutions are partial victims (d ≈ 0.65) — they lose metaphysical standing (high d) but retain political organizing capacity and arbitrage exit options (moderating the d somewhat). Medical professionals experience d ≈ 0.50-0.55 (symmetric) — they benefit from normative clarity but suffer cognitive cost when enforcing contradictory state mandates. The directionality is not distributive (resources flowing from one agent to another) but metaphysical and epistemic: the reading flows authority to pregnant persons and flows standing away from fetal advocates by rewriting the boundary of who is a legal subject. This creates chi values that systematically advantage institutional actors aligned with autonomy-based abortion rights and disadvantage religious conservative institutions whose authority depends on fetal moral status claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The birth reading resolves mandatrophy by explicitly accepting that it is a READING (one institutional articulation of contested boundaries) rather than a transparent recognition of moral fact. The constraint is Tangled Rope: it coordinates reproductive autonomy AND it extracts from fetal advocates. Both functions are real. The mandate is not 'choose one function' but 'acknowledge both.' The reading coordinates because pregnant persons and abortion-access infrastructure genuinely need a clear framework for decision-making. It extracts because fetal-advocate voices are systematically excluded from policy authority by the reading's epistemic closure. This is not a defect in the reading — it is the structural reality of any boundary-setting claim. The false summit detector will flag the analytical observer's mountain perspective, revealing that the 'natural boundary' framing masks the reading's contingency and beneficiary structure. The mandatrophy is resolved by transparency: state clearly that this is one reading of a contested kernel, not a law of nature. This moves the constraint from candidate for reclassification to fully analyzed tangled structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_alternative_readings,
    'Is the birth reading the only coherent boundary for legal personhood, or are alternative readings (conception, viability, brain activity) equally defensible as institutional choices?',
    'Comparative institutional analysis of three readings: conception_reading, viability_reading, birth_reading. Each has its own ε, beneficiary/victim structure, enforcement costs, and theater ratio. The resolution is to recognize that this is ONE reading of a contested kernel, not the uniquely correct reading.',
    'If alternative readings are equally coherent: the birth reading is a contingent institutional choice, not a natural law. The false summit detector will reclassify the mountain perspective as tangled_rope or snare depending on which reading is operative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_alternative_readings, conceptual, 'Whether birth reading is the only coherent personhood boundary or one of multiple defensible readings').

omega_variable(
    fetal_moral_status_metaphysics,
    'Does the birth reading''s claim that the fetus lacks moral status depend on a contentious metaphysical thesis about consciousness, potentiality, or relational ontology?',
    'Philosophical analysis of the implicit metaphysical commitments in the birth reading. Does it require: (a) consciousness as necessary for moral status? (b) actualized capacities vs. potential capacities? (c) independence as a criterion? Each commitment has metaphysical costs and alternatives.',
    'If the reading depends on contentious metaphysical claims: its apparent naturalness is an illusion created by institutional dominance. The reading is not a transparent recognition of moral fact but a specific metaphysical frame. This strengthens the omega about kernel alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fetal_moral_status_metaphysics, conceptual, 'Metaphysical commitments underlying the birth reading''s fetal moral status claim').

omega_variable(
    enforcement_geopolitical_fragmentation,
    'As legal enforcement of the birth reading fragments geopolitically (US abortion bans post-Dobbs, EU protection, Global South variation), does the constraint degrade from tangled_rope to piton?',
    'Temporal measurement of enforcement consistency and institutional coherence. If abortion legality becomes deterministically related to state residence rather than principled universal boundary, the reading''s enforcement mechanism has degraded to performance.',
    'If geopolitical fragmentation is permanent: the birth reading transitions from contested institutional principle to performative normativity (piton). The scaffold sunset accelerates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_geopolitical_fragmentation, empirical, 'Whether geopolitical fragmentation degrades birth reading to piton').

omega_variable(
    reproductive_technology_boundary_shift,
    'Do emerging reproductive technologies (artificial wombs, in vitro fetal development, gestational surrogacy) render the birth boundary incoherent or force redefinition of the personhood kernel?',
    'Empirical development of reproductive technology timelines. If artificial gestation becomes viable, does the birth reading collapse (the fetus can be removed without death) or shift to ''exit from dependence on a specific person''s body''?',
    'If technologies shift the boundary: the birth reading''s apparent clarity dissolves. The kernel may bifurcate into multiple readings based on technology class. This would create multiple constraint stories in a constraint family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reproductive_technology_boundary_shift, empirical, 'Whether reproductive technology shifts or collapses the birth boundary').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.68) structural (legal bans, resource barriers) or partially internalized (pregnant persons internalizing abortion stigma, medical providers internalizing conscience clauses)?',
    'Post-legalization suppression trajectory: if suppression persists after legal barriers are removed, reclassify portion as internalized. Measure via abortion stigma surveys and provider behavior in maximally permissive jurisdictions.',
    'If suppression is substantially internalized: the constraint''s effective suppression is higher than structural measures suggest. Pregnant persons carry the suppression with them even post-legalization, indicating the reading operates partly through internalized identity constraints rather than external enforcement alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in birth reading enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(birth_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(birt_tr_t0, birth_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(birt_tr_t15, birth_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(birt_tr_t30, birth_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(birt_be_t0, birth_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(birt_be_t15, birth_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(birt_be_t30, birth_reading, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(birth_reading, identity_coordination).
narrative_ontology:affects_constraint(birth_reading, conception_reading).
narrative_ontology:affects_constraint(birth_reading, viability_reading).

% DUAL FORMULATION NOTE:
% The personhood_boundary kernel has three main readings: birth_reading (this story), conception_reading, and viability_reading. Each reading is a separate constraint with its own ε, beneficiary/victim structure, and enforcement mechanisms. They are linked as a constraint family because they all operate on the same kernel and each reading's institutional dominance affects the others' feasibility. The birth_reading's current (2026) institutional dominance in Western law makes it the 'default' frame, but all three readings are structurally coherent and have historical/regional instances. The omegas in this story address why this reading was selected and what would shift the boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(birth_reading, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
