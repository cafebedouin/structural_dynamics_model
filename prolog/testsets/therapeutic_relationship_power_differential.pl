% ============================================================================
% CONSTRAINT STORY: therapeutic_relationship_power_differential
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_therapeutic_relationship_power_differential, []).

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
 *   constraint_id: therapeutic_relationship_power_differential
 *   human_readable: Power Differential in Therapeutic Relationships
 *   domain: interpersonal/clinical_psychology/professional_ethics
 *
 * SUMMARY:
 *   The therapeutic relationship power differential is a structural
 *   constraint arising from the asymmetry between a person seeking help in a
 *   vulnerable state and a credentialed professional with epistemic authority
 *   and control over the relationship frame. This constraint operates across
 *   all contexts where one party has superior knowledge, institutional
 *   sanction, and the ability to define the problem and solution: therapy,
 *   coaching, mentorship, pastoral counseling, medical encounters. The
 *   constraint exhibits genuine coordination function — therapists do help
 *   people develop insight, acquire skills, and navigate psychological
 *   difficulty — while simultaneously extracting through dependency creation,
 *   normalization of the therapist's authority over the client's
 *   self-understanding, and the creation of psychological costs to exit. The
 *   therapeutic ideal narrative naturalizes this extraction as necessary for
 *   healing. The constraint's extractiveness increases over the treatment
 *   interval (0.35 → 0.58) as dependency deepens; theater ratio also
 *   increases (0.35 → 0.55) as the performative maintenance of the
 *   'therapeutic alliance' becomes more central to justifying the
 *   relationship's continuation. The constraint is identity-locking: clients
 *   internalize the belief that the therapeutic relationship is necessary for
 *   their identity as someone who is 'working on themselves,' making exit
 *   psychologically experienced as identity-threatening rather than as simple
 *   choice to stop paying for a service.
 *
 * KEY AGENTS:
 *   - Vulnerable Client: Primary victim (powerless/identity_locked) — enters therapy in distress, seeking expert guidance; exits become identity-threatening as self-concept fuses with therapeutic role
 *   - Self-Aware Client: Secondary victim/beneficiary (moderate/constrained) — recognizes power differential but benefits from genuine therapeutic skill; bears costs of asymmetric authority and high exit costs
 *   - Competent Therapist: Primary beneficiary (institutional/arbitrage) — provides genuine help, economically compensated, experiences power differential as legitimate tool; can arbitrage to other clients/roles
 *   - Exploitative Therapist: Maximal extractor (powerful/arbitrage) — weaponizes power differential; boundary violations, gaslighting, sexual/financial exploitation; full exit optionality while client is trapped
 *   - Regulatory Profession: Organized gate-keeper (organized/constrained) — licensing boards, ethics codes, supervision requirements; attempts to scaffold the relationship but constrained by professional self-interest
 *   - Therapeutic Relationship Ideal: Institutional narrative (institutional/arbitrage) — the cultural myth that therapeutic relationships are inherently healing persists as theater despite evidence of failure; maintained through credentialing and funding institutions
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as genuinely hybrid: coordination + extraction, not reducible to either pure type
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(therapeutic_relationship_power_differential, 0.58).
domain_priors:suppression_score(therapeutic_relationship_power_differential, 0.68).
domain_priors:theater_ratio(therapeutic_relationship_power_differential, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(therapeutic_relationship_power_differential, extractiveness, 0.58).
narrative_ontology:constraint_metric(therapeutic_relationship_power_differential, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(therapeutic_relationship_power_differential, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(therapeutic_relationship_power_differential, tangled_rope).
narrative_ontology:human_readable(therapeutic_relationship_power_differential, "Power Differential in Therapeutic Relationships").
narrative_ontology:topic_domain(therapeutic_relationship_power_differential, "interpersonal/clinical_psychology/professional_ethics").

domain_priors:requires_active_enforcement(therapeutic_relationship_power_differential).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(therapeutic_relationship_power_differential, therapist_economic_position).
narrative_ontology:constraint_beneficiary(therapeutic_relationship_power_differential, therapist_epistemic_authority).
narrative_ontology:constraint_victim(therapeutic_relationship_power_differential, client_vulnerability).
narrative_ontology:constraint_victim(therapeutic_relationship_power_differential, client_autonomy_during_treatment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE CLIENT (SNARE) — Client enters therapy in crisis or distress, seeking help from perceived expert. Therapeutic dependency creates high suppression: exit costs include abandonment of treatment, fear of relapse, internalized belief that 'quitting therapy means I'm not committed to healing.' Identity fusion: the client's self-concept becomes constituted through the therapeutic relationship ('I am someone in therapy,' 'this therapist knows me better than anyone'). Structurally mobile (could stop attending) but identity-locked (cannot imagine doing so without identity dissolution). Extracted through unpaid emotional labor, disclosure of intimate material, normalization of the therapist's interpretations over client's self-knowledge.
constraint_indexing:constraint_classification(therapeutic_relationship_power_differential, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: SELF-AWARE CLIENT (TANGLED ROPE) — Client who recognizes the power differential but continues therapy for genuine benefit. Coordination function: therapist provides structured space for self-reflection, naming of patterns, skill development. Asymmetric extraction: therapist controls session framing, interpretation authority, pace, termination conditions. High cost to exit (loss of progress, relationship rupture, career/social costs of mental health disclosure). But not trapped — possesses enough epistemic authority to challenge therapist, seek second opinions, or leave despite costs.
constraint_indexing:constraint_classification(therapeutic_relationship_power_differential, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: COMPETENT THERAPIST (ROPE) — Therapist genuinely perceives the relationship as coordination: creating safe space, building trust, enabling client agency. Experiences power differential as necessary for therapeutic function (clients need to believe therapist has expertise). Has arbitrage exit options: can leave practice, refer clients, shift specialties. Extracted through legitimate income exchange, but extraction is not the primary motivation — helping is. The power differential is experienced as a tool, not an advantage.
constraint_indexing:constraint_classification(therapeutic_relationship_power_differential, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: EXPLOITATIVE THERAPIST (SNARE) — Therapist who weaponizes the power differential: boundary violations, sexual contact, financial exploitation, gaslighting of client concerns. Extraction is maximal and intentional. Client has trapped/identity_locked exit options; therapist has full arbitrage (can terminate, move to different client population, evade accountability). Suppression is extremely high: client fears loss of treatment, believes therapist's framing ('you're too fragile for truth,' 'this is part of your healing'), has internalized belief that the abuse is therapeutic. The constraint exists to maximize extraction.
constraint_indexing:constraint_classification(therapeutic_relationship_power_differential, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 5: REGULATORY PROFESSION (SCAFFOLD) — Professional bodies (APA, NASW, AMA), licensing boards, and accreditation standards attempt to regulate the power differential through codes of ethics, continuing education, supervision requirements, and complaint mechanisms. These create temporary structures that sunset power asymmetry violations when functioning well. Theater ratio is high (compliance theater often exceeds actual client protection), but the scaffold function is real: codes exist, enforcement mechanisms exist, and the norms gradually shift. Organized agents have constrained exit from the regulatory framework itself but agency in how rigorously it's applied.
constraint_indexing:constraint_classification(therapeutic_relationship_power_differential, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THERAPEUTIC RELATIONSHIP IDEAL (PITON) — The civilizational narrative that 'the therapeutic relationship is inherently healing' persists as institutional theater despite degraded function. The ideal relationship assumes: (1) client can safely disclose vulnerability, (2) therapist uses power responsibly, (3) power differential is transparent and bounded. In reality, institutional incentives (profit, credentialing, liability) often override these assumptions. The ideal relationship persists through institutional inertia — funding, training, and accreditation systems maintain the narrative that the relationship model works, despite abundant evidence of failure modes. Theater ratio is high because the maintenance of the 'therapeutic alliance' narrative is the primary function, not client protection.
constraint_indexing:constraint_classification(therapeutic_relationship_power_differential, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the analytical/civilizational perspective, therapeutic relationships coordinate genuine human needs (vulnerability, guidance, skill development) while extracting from clients through asymmetric power, epistemic authority, and dependency creation. The coordination function is real and necessary; the extraction is systematic and structural. This perspective rejects both the exploitation narrative (some therapists are genuinely helpful) and the ideal narrative (the power differential is not inherent to healing). The constraint is a hybrid: neither pure extraction nor pure coordination.
constraint_indexing:constraint_classification(therapeutic_relationship_power_differential, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(therapeutic_relationship_power_differential_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(therapeutic_relationship_power_differential, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(therapeutic_relationship_power_differential, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(therapeutic_relationship_power_differential, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(therapeutic_relationship_power_differential, TR),
    TR >= 0.70.

:- end_tests(therapeutic_relationship_power_differential_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The therapeutic relationship extracts through multiple mechanisms: (1) unpaid emotional labor — client provides raw material (vulnerable disclosure) that therapist uses to construct interpretations and demonstrate expertise; (2) dependency creation — client becomes psychologically dependent on the therapist's framing of their problems; (3) epistemic extraction — therapist's interpretation authority crowds out client's own self-knowledge ('the therapist knows me better than I know myself'); (4) temporal extraction — client becomes obligated to continue attending to justify past investment (sunk cost framing). However, extraction is not maximal (0.58, not 0.75+) because genuine benefit occurs: clients do develop skills, insight, and psychological flexibility. The extractiveness value reflects that benefit and extraction coexist. Suppression (0.68): High. Multiple mechanisms suppress exit: (1) Material: financial cost, insurance complexity, travel, therapeutic relationship loss. (2) Structural: belief that mental health requires ongoing professional intervention; fear of relapse. (3) Internalized: identity fusion where leaving therapy is experienced as identity failure; shame narratives; belief that termination means the client 'didn't work hard enough.' The high suppression reflects that all three mechanisms operate simultaneously. Theater ratio (0.55): Moderate-high. Therapeutic work includes genuine intervention (skill teaching, pattern naming, emotional processing) but also substantial performative content: (1) the ritual of the scheduled session itself becomes the healing agent rather than what's discussed; (2) the therapist's neutrality/unconditional regard becomes performed rather than authentically present; (3) insurance and outcome measurement create performance incentives (therapist documents improvement to justify continued sessions). The theater has increased over the measurement interval because the credential-maintenance function (therapist demonstrates expertise through subtle interpretation, withheld judgment) has become more central as the relationship deepens.
 *
 * PERSPECTIVAL GAP:
 *   The largest perspectival gap is between the vulnerable client's snare classification and the therapeutic ideal narrative's rope classification. Both describe the same constraint. The snare perspective arises from powerless/identity_locked/trapped — the client cannot exit despite wanting to, because the exit costs (therapeutic abandonment, identity loss, fear of relapse) are overwhelming. The rope perspective arises from the institutional therapist position with arbitrage — the therapist genuinely solves a coordination problem (client gets help, therapist gets income, the relationship enables both parties' goals) and experiences the power differential as a necessary tool, not an extraction advantage. The gap reveals that the constraint's classification depends fundamentally on which agent's position you occupy. Neither perspective is 'wrong' — they are measuring the constraint from opposite structural positions. The analytical observer's tangled rope classification acknowledges that both perspectives are correct and that the constraint genuinely exhibits both coordination and extraction functions.
 *
 * DIRECTIONALITY LOGIC:
 *   The vulnerable client's identity_locked exit option is the key structural feature. Unlike a materially trapped agent (constrained exit, trapped exit) who faces surmountable barriers, the identity_locked agent's self-concept is fused with the constraint. Exiting would require not just leaving therapy but reconstructing the identity that was constituted through therapy ('I am someone in therapy' → 'I am someone who quit therapy'). This is experienced as identity death, not just cost. The derivation chain produces: victim status (bears extraction costs) + identity_locked exit (cannot reimagine self outside the relationship) + moderate-powerless power level → d value in the 0.85-0.95 range → f(d) ≈ 1.28-1.42 → high experienced extractiveness. By contrast, the competent therapist has beneficiary status + arbitrage exit → d ≈ 0.15 → f(d) ≈ -0.01 → experienced extractiveness approaches zero (the therapist sees pure coordination). The gap between these two d values (0.15 vs 0.90) quantifies the perspectival disagreement: the same constraint's extractiveness ranges from effectively negative (rope) to maximally high (snare) depending on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The therapeutic relationship constraint resolves the mandatrophy through perspectival decomposition rather than false natural law detection. The constraint is genuinely tangled rope: it coordinates (therapist provides skilled help, client gains insight and agency) while extracting (power asymmetry, dependency creation, epistemic authority crowding out client self-knowledge). The mandatrophy-resolution question is not 'is this really extraction disguised as coordination?' but 'which perspective reveals the true structure?' The answer is: all of them. The vulnerable client's snare perspective is correct about the extraction mechanisms and their experience. The competent therapist's rope perspective is correct about the coordination function. The exploitative therapist's snare perspective (from the extractor side) is correct about weaponized power. The regulatory profession's scaffold perspective is correct about the nascent alternative pathways (peer support, community mental health, collective healing). The therapeutic ideal narrative's rope perspective is correct about genuine coordination but wrong about extraction. The analytical observer's tangled rope perspective is correct that both mechanisms operate. No single perspective is false; the constraint is irreducibly hybrid. The risk of false natural law appears in the therapeutic ideal narrative's claim that 'power differential is necessary for therapeutic change' — this naturalizes a contingent institutional arrangement. The analytical observer must flag this as a false summit: the power differential is culturally constructed, not inherent to human healing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    therapeutic_benefit_versus_extraction,
    'How much of the client''s improvement is attributable to therapeutic skill versus the placebo effect of believing in expert help and the regression-to-mean effects of time?',
    'Meta-analysis of therapy effectiveness controlling for client expectations, time elapsed, and natural recovery rates. Comparison of outcomes across therapist skill levels in randomized settings. Long-term follow-up tracking of clients who terminate therapy vs those who continue.',
    'If therapeutic benefit is high (>70% above placebo): coordination function dominates, extraction is justified as coordination cost. If therapeutic benefit is low (<30% above placebo): coordination function is illusion, constraint becomes snare for all perspectives. If mixed (30-70%): tangled rope classification is confirmed — genuine benefit + systematic extraction coexist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(therapeutic_benefit_versus_extraction, empirical, 'Therapeutic benefit attribution: skill versus placebo versus natural recovery').

omega_variable(
    identity_lock_mechanism_specificity,
    'Is the client''s inability to exit the therapeutic relationship due to material costs (constrained), structural entrapment (trapped), or identity fusion where the self-concept is constituted through the therapeutic role (identity_locked)?',
    'Post-termination trajectory analysis: does the suppression persist after the relationship ends? If yes, suppression was partially internalized (identity_locked component). If no, suppression was entirely structural (constrained/trapped). Survey client narratives about imagined exit (''I can''t leave because...'' — financial/social costs vs ''I can''t leave because I am...'' — identity statement).',
    'If primarily constrained: exit barriers are material, reducible through policy (insurance coverage, sliding scale, referral networks). If primarily trapped: client has no agency regardless of policy. If primarily identity_locked: client''s self-concept requires therapeutic frame; exit requires identity reconstruction, much higher post-termination risk. Classification and policy response differ substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_specificity, empirical, 'Specificity of exit barriers: material versus identity-based').

omega_variable(
    supervisor_gating_effectiveness,
    'Do clinical supervision, peer consultation, and ethics review boards actually prevent boundary violations and exploitative dynamics, or are they primarily theater that legitimizes the relationship?',
    'Complaint data: fraction of violations caught by supervision vs reported by clients post-termination. Supervisor blind audits: how many recordings reviewed per therapist per year? What fraction of boundary-pushing gets documented vs normalized as clinical judgment? Outcome data: do supervised therapists show lower client harm rates?',
    'If gating is effective (>70% of violations caught internally): scaffold perspective is confirmed — regulatory mechanisms have real teeth. If gating is ineffective (<20%): scaffold is aspirational, piton is more accurate — the machinery persists without function. If mixed (20-70%): gating works for obvious violations but misses systemic exploitation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supervisor_gating_effectiveness, empirical, 'Whether supervision mechanisms prevent boundary violations').

omega_variable(
    power_differential_necessity,
    'Is the power differential inherent to effective therapy (therapist must be perceived as expert, authority, guide) or a contingent institutional feature that could be redesigned?',
    'Outcome comparison: peer support models vs therapist-led models. Co-therapy models with shared authority. Client-directed therapy with therapist as tool (client sets goals, controls sessions). Effectiveness data across models controlling for client severity, therapist training, and follow-up duration.',
    'If differential is necessary: power asymmetry is inherent coordination cost (rope or tangled rope from all perspectives). If differential is contingent: alternative models may eliminate extraction without losing coordination benefit. If partially necessary (helps early therapy, becomes obstacle later): suggests staged relationship models where differential decreases over time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(power_differential_necessity, empirical, 'Whether power differential is inherent to therapeutic effectiveness').

omega_variable(
    interpersonal_constraint_decomposition,
    'Should this constraint be decomposed into multiple structurally distinct stories (therapeutic alliance coordination vs professional boundary enforcement vs trauma reenactment dynamics)?',
    'ε-invariance test: do these three observable dimensions have different extractiveness values? Therapeutic alliance (coordination of safety, skill development): ε ≈ 0.25. Professional boundaries (enforcement of ethical constraints): ε ≈ 0.45. Trauma reenactment (unconscious replay of power dynamics): ε ≈ 0.72. If yes, decompose into separate stories linked by network.affects_constraints.',
    'Current story treats therapeutic relationship as unified. Decomposition would reveal different extraction mechanisms with different policy interventions. Alliance-focused therapy needs different safeguards than boundary-enforcement or trauma-aware work.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpersonal_constraint_decomposition, conceptual, 'Whether to decompose therapeutic relationship into distinct constraints').

omega_variable(
    suppression_internalization_degree,
    'Of the measured suppression (0.68), what fraction is structural (external barriers: cost, geography, liability fear) versus internalized (client beliefs that staying in the relationship is necessary for healing)?',
    'Post-termination tracking: does client describe relief (''finally free'') or grief-with-guilt (''I''m failing myself by leaving'')? Does suppression persist after exit? Internalized suppression shows as continued self-blame, shame about termination, difficulty trusting own judgment. Structural suppression shows as relief plus practical barriers to finding alternative care.',
    'If high internalization (>60%): constraint exhibits strong identity_lock component; client carries suppression even after structural relationship ends. Indicates deep cognitive capture. If low internalization (<30%): suppression is primarily material; policy addressing cost/access may reduce constraint significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_degree, empirical, 'Degree to which suppression is internalized versus structural').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(therapeutic_relationship_power_differential, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ther_tr_t0, therapeutic_relationship_power_differential, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ther_tr_t6, therapeutic_relationship_power_differential, theater_ratio, 6, 0.45).
narrative_ontology:measurement(ther_tr_t12, therapeutic_relationship_power_differential, theater_ratio, 12, 0.52).
narrative_ontology:measurement(ther_tr_t18, therapeutic_relationship_power_differential, theater_ratio, 18, 0.55).

% Extraction over time
narrative_ontology:measurement(ther_be_t0, therapeutic_relationship_power_differential, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ther_be_t6, therapeutic_relationship_power_differential, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(ther_be_t12, therapeutic_relationship_power_differential, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(ther_be_t18, therapeutic_relationship_power_differential, base_extractiveness, 18, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(therapeutic_relationship_power_differential, attachment_coordination).
narrative_ontology:affects_constraint(therapeutic_relationship_power_differential, therapeutic_boundary_violations).
narrative_ontology:affects_constraint(therapeutic_relationship_power_differential, trauma_reenactment_in_therapy).
narrative_ontology:affects_constraint(therapeutic_relationship_power_differential, therapeutic_alliance_quality).

% DUAL FORMULATION NOTE:
% The therapeutic relationship constraint should be decomposed into at least three structurally distinct stories: (1) therapeutic_alliance_coordination (ε ≈ 0.25) — genuine skill-based help and emotional support; (2) therapeutic_boundary_enforcement (ε ≈ 0.45) — the regulation of professional/personal boundaries; (3) therapist_dependency_mechanism (ε ≈ 0.72) — the unconscious replay of power dynamics and trauma patterns within the therapeutic frame. Each story has different ε values, different victim/beneficiary structures, and different policy interventions. The current story treats the therapeutic relationship as unified; decomposition would reveal why the constraint exhibits all six types across perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(therapeutic_relationship_power_differential, powerful, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
