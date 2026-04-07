% ============================================================================
% CONSTRAINT STORY: professional_identity_lock
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_professional_identity_lock, []).

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
 *   constraint_id: professional_identity_lock
 *   human_readable: Professional Identity Lock
 *   domain: organizational/institutional/interpersonal
 *
 * SUMMARY:
 *   Professional identity lock occurs when a practitioner's self-concept
 *   becomes fused with their professional role and credentials. The agent is
 *   structurally mobile — they have financial resources, alternative
 *   employment options, no legal barriers to exit — but functionally trapped
 *   by identity fusion. The binding mechanism is cognitive rather than
 *   material: exit would require not just paying a cost but *becoming a
 *   different person*. This constraint exhibits the full spectrum of DR
 *   classifications because the same institutional arrangement (professional
 *   licensing, credentialing requirements, career path dependence) appears as
 *   pure coordination (institutional perspective), mixed
 *   coordination-extraction (moderate perspective), pure extraction (locked
 *   perspective), a temporary barrier being eroded by alternatives (reform
 *   coalition), a degraded ritual (guild system), and a false natural law
 *   (naive analytical view). The constraint's extractiveness has increased
 *   over the 20-year interval as credential inflation has raised barriers and
 *   professional identity has become more rigidly defined. Theater ratio has
 *   risen as credentialing rituals have become more elaborate relative to
 *   their actual verification function.
 *
 * KEY AGENTS:
 *   - Identity-Locked Practitioner: Primary victim (powerless/identity_locked) — structurally mobile but identity-fused; cannot imagine themselves outside the role; bears full extraction
 *   - Aware Practitioner: Secondary victim (moderate/constrained) — recognizes the lock intellectually but faces high exit costs; mixed experience of coordination and extraction
 *   - Professional Institution: Primary beneficiary (institutional/arbitrage) — licensing bodies, professional associations; experience the constraint as coordination; benefit from standardization and controlled entry
 *   - Captured Regulator: Inter-institutional actor (institutional/constrained) — regulatory body whose identity has fused with the field it oversees; participates in extraction while maintaining coordination facade
 *   - Reform Coalition: Organized challengers (organized/constrained) — alternative-credential advocates, new entrants, digital credential platforms; perceive a sunset as alternatives mature
 *   - Professional Guild System: Institutional structure (institutional/arbitrage) — licensing boards, credentialing rituals; persists through inertia (piton perspective)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent features of professional knowledge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(professional_identity_lock, 0.52).
domain_priors:suppression_score(professional_identity_lock, 0.48).
domain_priors:theater_ratio(professional_identity_lock, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(professional_identity_lock, extractiveness, 0.52).
narrative_ontology:constraint_metric(professional_identity_lock, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(professional_identity_lock, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(professional_identity_lock, tangled_rope).
narrative_ontology:human_readable(professional_identity_lock, "Professional Identity Lock").
narrative_ontology:topic_domain(professional_identity_lock, "organizational/institutional/interpersonal").

domain_priors:requires_active_enforcement(professional_identity_lock).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(professional_identity_lock, institutional_employers).
narrative_ontology:constraint_beneficiary(professional_identity_lock, professional_gatekeepers).
narrative_ontology:constraint_victim(professional_identity_lock, identity_locked_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE LOCKED PRACTITIONER (SNARE) — The professional whose identity is constituted through the career path. Cannot imagine themselves outside this role. Exit would require abandoning the self-concept built over decades. Structurally mobile (has alternative employment options, financial resources, no legal barriers) but functionally trapped by identity fusion. The binding mechanism is cognitive — the agent's identity frame makes exit literally unthinkable from within.
constraint_indexing:constraint_classification(professional_identity_lock, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: THE AWARE PRACTITIONER (TANGLED ROPE) — Recognizes the identity lock while remaining partially bound by it. Understands the constraint rationally but faces high costs to exit: social penalties, career damage, financial loss, relocation burden. The constraint coordinates continued professional contribution (genuine function) while extracting from the practitioner's autonomy and life choices (asymmetric cost).
constraint_indexing:constraint_classification(professional_identity_lock, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PROFESSIONAL INSTITUTION (ROPE) — Experiences the constraint as pure coordination. Licensing bodies, professional associations, and employers benefit from identity lock as a coordination mechanism that aligns practitioner behavior with institutional standards. From this perspective, the constraint solves a collective action problem: ensuring practitioners maintain competence and ethical standards. Net beneficiary experiencing low extraction.
constraint_indexing:constraint_classification(professional_identity_lock, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE CAPTURED REGULATOR (TANGLED ROPE) — A regulatory body or licensing agency whose institutional identity has fused with the professional field it was created to oversee. The regulator is structurally constrained (legislative mandate, organizational culture, career paths through the regulated field). Coordinates legitimate credentialing standards while participating in extraction mechanisms that protect incumbent practitioners from competition.
constraint_indexing:constraint_classification(professional_identity_lock, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE REFORM COALITION (SCAFFOLD) — Professional outsiders, new entrants, and alternative-credential advocates see the identity lock as a temporary barrier. Digital credentials, lateral entry programs, and alternative professional pathways are creating exits that bypass the traditional lock. Organized agents perceive a sunset: as alternative pathways mature, the identity lock's extraction power decays. Theater-driven transition with real coordination function underneath.
constraint_indexing:constraint_classification(professional_identity_lock, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE PROFESSIONAL GUILD SYSTEM (PITON) — The institutional apparatus (licensing boards, professional associations, credentialing rituals) persists through inertia despite diminished function. Digital credentials and skill-based hiring are eroding the guild's gatekeeping role, but the licensing ritual persists because alternatives haven't fully replaced it. High theater ratio reflects performative credentialing — the ritual maintains social legitimacy even as its verification function atrophies.
constraint_indexing:constraint_classification(professional_identity_lock, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some version of professional identity attachment appears inherent to human social organization. The observer might naturalize this as an unchangeable feature of how specialized knowledge is transmitted and enforced. However, the structural data contradicts the mountain classification — the engine's false summit detector identifies this as naturalization. Professional identity lock is contingent on specific institutional arrangements (licensure, credentialing monopoly, career path dependence), not a law of nature.
constraint_indexing:constraint_classification(professional_identity_lock, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(professional_identity_lock_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(professional_identity_lock, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(professional_identity_lock, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(professional_identity_lock, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(professional_identity_lock, TR),
    TR >= 0.70.

:- end_tests(professional_identity_lock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The lock extracts from practitioners through credential inflation (rising costs and time requirements), credential lock-in (sunk costs in specific credentialing pathway), and market concentration (restricted entry protecting incumbent wages and opportunity). The extraction is real but not total — practitioners benefit from professional status, network access, and collective standard-setting. The upward trajectory reflects intensifying credential requirements and rising barriers to lateral entry as professional gatekeeping has strengthened. Suppression (0.48): Moderate. Significant barriers to exit include financial sunk costs (years of training), social penalties (professional network loss, status downgrade), career penalties (resume gaps, skill atrophy), and psychological barriers (identity fusion preventing even conception of exit). However, suppression is not total — some practitioners do exit, and external pathways exist though narrow. Theater ratio (0.65): Moderately high and rising. Professional credentialing increasingly relies on theatrical validation — licenses, certifications, formal credentials — that signal competence to employers but whose actual verification function has atrophied relative to their cost and complexity. Continuing education requirements, professional conferences, and credentialing exams often maintain social legitimacy without proportional quality assurance. Theater has risen as digital credentials and skill-based hiring have begun eroding the license's gatekeeping function, forcing the traditional system to rely more heavily on ritual legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural arrangement produces divergent classifications. The professional institution sees the lock as legitimate coordination — ensuring practitioners maintain standards, preventing incompetence from harming consumers. The locked practitioner sees extraction and suppression — their autonomy and life choices are constrained, and they cannot imagine escaping. The captured regulator sees mixed coordination and extraction — genuinely overseeing competence standards while participating in incumbent protection. The reform coalition sees a temporary barrier with a sunset — alternative pathways are real and growing. The guild system (piton) sees its own degrading function — credentialing maintains ritual legitimacy even as its verification role atrophies. The naive analytical observer risks seeing a natural law — professional identity attachment as inherent to specialization — but the structural data reveals this as contingent on specific institutional arrangements (licensing monopoly, career path dependence, credential inflation).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's structural position relative to the identity lock. Professional institutions (beneficiaries with arbitrage exit) experience low d: they set standards and collect licensing fees with minimal extraction cost. Locked practitioners (victims with identity_locked exit) experience high d: they bear full extraction and cannot exercise structural mobility because their identity frame makes exit unthinkable. Aware practitioners (victims with constrained exit) experience moderate-high d: they perceive extraction and barriers but maintain some agency. The captured regulator (institutional/constrained) experiences moderate d: they benefit institutionally but are also constrained by the field's norms. The reform coalition (organized/constrained) experiences lower d because they have agency and see an exit path (sunset). The piton perspective derives from theater ratio (0.65) rather than from high experienced extraction. The false mountain at the analytical level is revealed by the structural data: the constraint is contingent on institutions (licensing, credentialing monopoly), not universal.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how identity lock weaponizes coordination. The professional institution legitimately coordinates competence standards (rope function). But the same institutional apparatus enables extraction through credential lock-in, market concentration, and identity fusion (snare and tangled rope functions coexist). The mandatrophy is resolved by distinguishing the actual coordination needed (verifying competence) from the extractive mechanisms layered on top (credential inflation, lateral entry barriers, professional network gatekeeping). The constraint is tangled rope: genuine coordination + asymmetric extraction coexist and cannot be separated. The piton perspective reveals that theater is rising as the coordination function weakens — the rituals persist even as their functional verification decays. The reform coalition's scaffold perspective is the exit: alternative credentialing pathways (skill-based hiring, digital credentials, portfolio verification) provide coordination function without identity lock, suggesting the lock is institutional contingency rather than coordination necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_constrained_boundary,
    'At what cost threshold does a structurally mobile agent become effectively trapped by identity fusion rather than material barriers?',
    'Longitudinal study of practitioners who attempted exit: What was the stated reason for returning? Was it external cost (lost income, social penalty) or internal cognitive barrier (couldn''t sustain new identity)? Compare agents who exited successfully (identity frame shifted) vs those who returned (identity fusion reasserted).',
    'If primarily external: classification should use ''constrained'' rather than ''identity_locked'' for more accurate directionality. If primarily internal: identity_locked is correct, but the suppression metric should be reclassified as partially internalized rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_boundary, empirical, 'Boundary between material constraint and identity lock').

omega_variable(
    alternative_credential_sufficiency,
    'Do alternative credentialing pathways (digital credentials, skill-based hiring, portfolio verification) actually provide functional equivalents to traditional licensing, or do they serve different labor markets?',
    'Outcome comparison: earnings, employer prestige, job security, advancement trajectory for alternative-credentialed practitioners vs traditionally licensed practitioners in the same role. Network analysis of which employers accept alternative credentials.',
    'If functionally equivalent: scaffold sunset is real — the identity lock''s coordination function is being replaced, and extraction will decline over time. If different markets: alternative pathways create parallel systems with different identity requirements, potentially deepening rather than resolving the lock.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_credential_sufficiency, empirical, 'Whether alternative credentials provide functional equivalents to licensing').

omega_variable(
    internalized_vs_structural_suppression,
    'What proportion of the measured suppression (0.48) is structural (external barriers to exit) vs internalized (cognitive patterns that persist after barrier removal)?',
    'Post-exit trajectory analysis: practitioners who exit the profession — do career penalties persist indefinitely (structural) or do they dissipate as new identity frame stabilizes (internalized)? Compare suppression scores for practitioners who exited voluntarily vs those forced out by external events.',
    'If primarily internalized: the constraint carries suppressive force even after material barriers are removed. The identity lock is more severe than structural cost measures suggest. If primarily structural: suppression score is accurate, and removing material barriers would substantially reduce experienced constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Proportion of suppression that is internalized vs structural').

omega_variable(
    gatekeeping_vs_coordination_function,
    'Does professional licensing genuinely coordinate competence and ethics (real function) or primarily restrict competition (extraction mechanism)?',
    'Comparative analysis: regulatory outcomes across jurisdictions with strict licensing (high gatekeeping) vs permissive credentials (low gatekeeping). Measure: practitioner quality metrics, consumer safety/satisfaction, innovation rate, job market concentration, wage premia.',
    'If coordination dominates: the rope perspective is correct — identity lock serves genuine collective action function. If gatekeeping dominates: the snare and tangled_rope perspectives are correct — the identity lock is primarily extractive with thin coordination veneer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_vs_coordination_function, empirical, 'Whether licensing coordinates competence or restricts competition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(professional_identity_lock, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prof_id_tr_t0, professional_identity_lock, theater_ratio, 0, 0.5).
narrative_ontology:measurement(prof_id_tr_t10, professional_identity_lock, theater_ratio, 10, 0.58).
narrative_ontology:measurement(prof_id_tr_t20, professional_identity_lock, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(prof_id_be_t0, professional_identity_lock, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prof_id_be_t10, professional_identity_lock, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(prof_id_be_t20, professional_identity_lock, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(professional_identity_lock, identity_coordination).
narrative_ontology:affects_constraint(professional_identity_lock, credential_inflation).
narrative_ontology:affects_constraint(professional_identity_lock, professional_gatekeeping).
narrative_ontology:affects_constraint(professional_identity_lock, lateral_entry_barriers).

% DUAL FORMULATION NOTE:
% Professional identity lock is downstream of specific credentialing systems (medical licensing, law bar, engineering PE) but represents a distinct structural constraint operating at the identity/psychological level. Decomposition into domain-specific stories recommended: medical_identity_lock (specialty lock within physicians), legal_identity_lock (partnership track lock in law firms), academic_identity_lock (tenure track lock in universities). Each subdomain has different extractiveness values reflecting domain-specific credential inflation rates, lateral entry availability, and identity fusion mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(professional_identity_lock, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
