% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__overdetermined_composite_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dueling_disappearance_mechanism__overdetermined_composite_reading
 *   human_readable: Overdetermined-Composite Reading of Dueling's Disappearance
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   By the late nineteenth century, dueling had all but vanished from
 *   American elite life. This reading holds that no single explanation — not
 *   legal prohibition alone, not the rise of courts and commercial credit
 *   institutions alone, not a cultural shift from honor to dignity alone, not
 *   Civil War trauma alone — is sufficient by itself to fully account for the
 *   decline; rather, each condition was independently sufficient to
 *   substantially suppress dueling, and they operated simultaneously and
 *   reinforced one another, producing an overdetermined causal outcome. The
 *   composite reading treats the four mechanisms as jointly constituting a
 *   single tangled institutional transition rather than crediting a single
 *   load-bearing cause.
 *
 * KEY AGENTS:
 *   - state_legal_monopolists: Primary agenda-setter (institutional/arbitrage) — administers criminalization and the courts substituting for private violence
 *   - commercial_credit_institutions: Beneficiary (organized/mobile) — gains from bureaucratic reputation replacing honor-code reputation
 *   - emerging_professional_middle_class: Beneficiary (moderate/mobile) — gains relative status as aristocratic honor capital devalues
 *   - postbellum_reconciliationist_elites: Beneficiary and secondary agenda-setter (powerful/constrained) — uses anti-dueling sentiment for national reconciliation
 *   - displaced_honor_culture_gentry: Primary target (moderate/trapped) — loses core status-defense mechanism with no substitute
 *   - southern_planter_class_remnants: Primary target (moderate/trapped) — bears the compounded cost of all four converging mechanisms at once
 *   - historical_sociologists_of_violence: Analytical observer — assesses whether the mechanisms are genuinely non-separable or a shared symptom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.42).
domain_priors:suppression_score(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.55).
domain_priors:theater_ratio(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__overdetermined_composite_reading, "Overdetermined-Composite Reading of Dueling's Disappearance").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__overdetermined_composite_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__overdetermined_composite_reading, '027dc4cd-faf3-4139-9ce7-35a9ac179720').
narrative_ontology:cs_kernel_codification('027dc4cd-faf3-4139-9ce7-35a9ac179720', distributed).
narrative_ontology:cs_authority_grounding('027dc4cd-faf3-4139-9ce7-35a9ac179720', distributed).
narrative_ontology:cs_reading_relation('027dc4cd-faf3-4139-9ce7-35a9ac179720', dueling_disappearance_mechanism__contraction_reading, influences).
narrative_ontology:cs_reading_relation('027dc4cd-faf3-4139-9ce7-35a9ac179720', dueling_disappearance_mechanism__institutional_displacement_reading, influences).
narrative_ontology:cs_axiom('027dc4cd-faf3-4139-9ce7-35a9ac179720', foundational, causal_plurality_non_separability_thesis).
narrative_ontology:cs_axiom_status(causal_plurality_non_separability_thesis, holdable).
narrative_ontology:cs_axiom_grounding('027dc4cd-faf3-4139-9ce7-35a9ac179720', causal_plurality_non_separability_thesis, empirically_contingent).
narrative_ontology:cs_axiom('027dc4cd-faf3-4139-9ce7-35a9ac179720', secondary, no_single_dominant_mechanism_claim).
narrative_ontology:cs_axiom_status(no_single_dominant_mechanism_claim, holdable).
narrative_ontology:cs_axiom_grounding('027dc4cd-faf3-4139-9ce7-35a9ac179720', no_single_dominant_mechanism_claim, empirically_contingent).
narrative_ontology:cs_reference_frame('027dc4cd-faf3-4139-9ce7-35a9ac179720', single_mechanism_historical_causation_norm).
narrative_ontology:cs_drift_state('027dc4cd-faf3-4139-9ce7-35a9ac179720', post_quantitative_historiography_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('027dc4cd-faf3-4139-9ce7-35a9ac179720', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, state_legal_monopolists).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, commercial_credit_institutions).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, emerging_professional_middle_class).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, postbellum_reconciliationist_elites).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, displaced_honor_culture_gentry).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, unresolved_reputational_disputants).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, southern_planter_class_remnants).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__overdetermined_composite_reading, multiple_sufficient_causation_doctrine).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__overdetermined_composite_reading, state_monopoly_on_violence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State legislatures and courts criminalize dueling and expand civil remedies for defamation and assault, positioning the state as the sole legitimate arbiter of honor disputes. They gain jurisdiction and legitimacy each time a duel is prosecuted or a libel suit substitutes for a challenge, and they administer the prohibition apparatus that makes exit from formal adjudication costly.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, state_legal_monopolists, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, state_legal_monopolists, beneficiary).

% Banks, credit-rating bureaus, and commercial exchanges need predictable reputational signaling that does not depend on a gentleman surviving a pistol duel to remain creditworthy. They benefit from the shift toward bureaucratic reputation mechanisms (credit records, litigation, professional societies) that dueling's decline makes possible, and face no meaningful exit pressure themselves.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, commercial_credit_institutions, beneficiary,
    organized, generational, mobile, national).

% Doctors, lawyers, merchants, and clerks whose status derives from credentialing and institutional standing rather than aristocratic honor. They gain relative standing as dueling's aristocratic prestige economy erodes, since the new mechanisms (professional licensing boards, courts) reward exactly the kind of institutional legitimacy they hold instead of birth-linked honor capital.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, emerging_professional_middle_class, beneficiary,
    moderate, biographical, mobile, national).

% Northern and southern elites seeking postwar national reconciliation actively discourage dueling as a symbol of the sectional violence culture they wish to bury alongside Civil War trauma. They administer social and reputational sanctions against dueling within elite clubs and press, converting anti-dueling sentiment into a unifying national narrative that also serves their reconciliation project.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, postbellum_reconciliationist_elites, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, postbellum_reconciliationist_elites, agenda_setter).

% Southern and some northern gentlemen whose entire status economy depended on personal honor defended by the threat of the duel. As legal prohibition, institutional courts, and cultural stigma converge simultaneously, they lose their primary mechanism for defending reputation and cannot simply substitute an equivalent status-defense system; many are trapped between an obsolete code and institutions that do not recognize their claims.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, displaced_honor_culture_gentry, payer,
    moderate, biographical, trapped, regional).

% Individuals with genuine grievances who previously might have used the duel's ritualized, bounded violence to resolve disputes now face slower, costlier, and less socially legible civil litigation, or no functional remedy at all if they lack the capital or standing to litigate. The overdetermined convergence of causes leaves this population without transitional support.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, unresolved_reputational_disputants, payer,
    powerless, immediate, constrained, local).

% Postbellum planters whose prewar social order (in which dueling was one visible marker of caste-based honor) collapses under combined military defeat, Civil War trauma, federal legal reconstruction, and the encroachment of northern commercial-legal norms. They bear the compounded cost of the convergence — economic ruin plus the loss of the honor-defense apparatus simultaneously — without a single identifiable actor to hold responsible.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, southern_planter_class_remnants, payer,
    moderate, generational, trapped, regional).

% Scholars examining the multi-causal collapse of dueling as a case study in overdetermined institutional change, comparing it to other honor-culture transitions cross-nationally to assess whether any single mechanism was load-bearing or whether the causes were genuinely jointly sufficient and non-separable.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, historical_sociologists_of_violence, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The composite of legal prohibition, institutional substitution, and elite reconciliation coordinates a transition away from private violence as a reputational-dispute mechanism toward state- and market-mediated reputation systems, reducing elite mortality and standardizing dispute resolution across a fracturing and then reunifying nation.
% TRANSFER_FUNCTION: Moves reputational-adjudication authority and the legitimacy that comes with it from an aristocratic honor code administered by dueling gentlemen to state courts, commercial credit institutions, and professional bodies — and moves social status away from birth-linked honor capital toward institutionally credentialed standing.
% ABSENT_VOICES: The displaced gentry and their honor-culture defenders are rarely given a corroborating voice in the historical record beyond apologetic memoir literature; freedpeople and non-elite populations who never had access to dueling as a dispute mechanism in the first place are entirely absent from the causal story, since the composite reading centers elite male reputational economies exclusively.
% DISAPPEARANCE_RATIONALE: If any single one of the four conditions (legal prohibition, institutional modernization, cultural shift, Civil War trauma) had been absent, historians dispute whether dueling would have persisted via the remaining three sufficient pathways or whether the composite was genuinely necessary in aggregate. The overdetermination itself is the contested claim: proponents say removing any one cause leaves the outcome unchanged (truly overdetermined); critics say the four causes were correlated symptoms of a single underlying modernization process, not independent sufficient conditions, in which case removing the shared root cause would have mattered.
% FOUNDING_PROBLEM: Elite honor cultures needed a mechanism to defend reputation and status claims outside formal law in a society where formal law was weak, contested, or seen as beneath gentlemen; dueling filled that gap by providing a ritualized, self-administered violence-backed reputational remedy.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the beneficiary institutions (state courts, credit bureaus) corroborate that formal legal remedies for defamation, contract, and assault had become sufficiently robust and accessible by the late 19th century that the honor-code's original function was substantially obsolete; comparative sociologists of violence independently corroborate the same displacement pattern in other national contexts (German Mensur decline, British duel decline post-1840s) absent any single U.S.-specific cause, supporting an assessment external to any of the U.S. beneficiary groups.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__overdetermined_composite_reading, contested).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).
:- end_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42 at interval end) because the composite reading does not identify a single concentrated extractor — the four converging mechanisms distribute gains across state institutions, commercial actors, the professional middle class, and reconciliationist elites, none of which captures a dominant share. Suppression is authored higher (0.55) because legal prohibition contributes genuine coercive force (criminal statutes, court enforcement) layered onto the softer cultural and institutional pressures; this is a case where suppression is structurally real even though extraction is diffuse. Theater ratio rises across the interval (0.1 to 0.4) because as actual dueling incidence collapses, residual anti-dueling legislation, elite social sanctioning rituals, and commemorative reconciliation rhetoric increasingly perform continued vigilance against a practice that has already become marginal — the enforcement apparatus outlives the behavior it targets.
 *
 * PERSPECTIVAL GAP:
 *   From the state and commercial-institution seats, the constraint appears as successful, overdetermined modernization — multiple independent forces confirming the same correct outcome. From the displaced gentry and planter-remnant seats, the same convergence appears as an inescapable pincer: no single opponent to resist or negotiate with, because the causal architecture itself is plural and non-separable. The engine computing different per-seat types from the same structural data is the intended signature of an overdetermined-causation reading — there is no single locus of extraction to contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (state legal monopolists, commercial credit institutions, the professional middle class, reconciliationist elites) are structurally positioned to gain from ANY of the four converging mechanisms, which is precisely what makes this a tangled_rope rather than a rope: each beneficiary group did not create the overdetermination, but each captures value from the composite outcome regardless of which specific mechanism was doing the causal work in a given case. Victims — displaced gentry and planter-class remnants — bear costs specifically because their status economy depended on the practice being viable, and they lack the option of substituting an equivalent honor-defense mechanism once dueling is delegitimized on four fronts simultaneously rather than one. Unresolved reputational disputants are victims of the transition's incompleteness: bounded, ritualized dispute resolution is removed before an accessible substitute is available to non-elite parties.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a self-administered reputational remedy where formal law was weak) is dead by the interval's end — courts, credit institutions, and professional bodies had matured into adequate substitutes. But the anti-dueling legal and social apparatus (criminal statutes, club expulsion codes, editorial condemnation rituals) persisted and even intensified in symbolic form after the practical need had passed, which is why theater_ratio rises independently of the practice's near-total disappearance. This is not mislabeled pure extraction, however: the composite reading holds that coordination value (reducing elite mortality, standardizing dispute resolution) was genuinely present throughout, alongside asymmetric costs falling on displaced honor-culture actors — hence tangled_rope rather than snare or pure piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_overdetermination_vs_shared_root_cause,
    'Were the four conditions (legal prohibition, institutional modernization, cultural shift, Civil War trauma) truly independent and each individually sufficient, or were they correlated symptoms of a single underlying modernization process (e.g., state-building and capitalist market integration) that would have produced the same outcome through whichever channel happened to be available?',
    'Comparative historical analysis: examine cases where fewer than four conditions were present (e.g., regions with cultural shift but weak legal prohibition, or legal prohibition without significant Civil War trauma) and observe whether dueling declined at a comparable rate. If decline tracks the presence of any one condition regardless of the others, overdetermination is supported; if decline requires the conjunction, a shared-root-cause reading is better supported and this constraint''s tangled_rope structure should be revisited.',
    'If the conditions are not truly independent, this reading''s claim of non-separable causal pathways collapses into a single-mechanism story resembling one of the sibling readings, and the tangled_rope classification (multiple distinct beneficiary mechanisms) may need to be resolved into whichever single mechanism actually drove the outcome.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_overdetermination_vs_shared_root_cause, conceptual, 'Whether the four causal conditions are genuinely independent-sufficient or symptoms of one root process.').

omega_variable(
    victim_set_indeterminacy,
    'Because ε is not cleanly separable across the four mechanisms, which victim group bore the dominant share of cost — displaced gentry harmed primarily by cultural delegitimization, or planter-class remnants harmed primarily by legal-institutional displacement compounded with war trauma?',
    'Micro-level case studies tracing individual dueling participants and their subsequent social/economic trajectories, coded by which of the four mechanisms was most proximate to their specific decline in status or dispute-resolution options.',
    'If one mechanism dominates for most affected individuals, the composite reading''s victim set should be narrowed and the constraint may be better modeled as extraction concentrated through a single channel rather than diffusely tangled — pushing the classification toward snare or toward one of the sibling single-mechanism readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_indeterminacy, empirical, 'Whether the victim set is genuinely diffuse across four mechanisms or dominated by one.').

omega_variable(
    reading_framing_underdetermination,
    'Is the overdetermined-composite framing itself a defensible independent reading of the kernel, or is it a residual category invoked whenever a single-mechanism story (contraction or institutional displacement) cannot be cleanly established — i.e., is overdetermination a positive causal claim or an admission of analytical failure to identify the dominant mechanism?',
    'Assess whether historians who hold the composite reading do so on positive evidence of simultaneous independent sufficiency (e.g., regional variation studies showing multiple pathways each independently correlating with decline) versus simply defaulting to ''many factors'' language when a single dominant cause proves difficult to isolate.',
    'If the composite reading is a residual/default position rather than a positive causal thesis, its claimed_type and ε should be treated with lower confidence relative to the sibling readings, which make more specific and falsifiable single-mechanism claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Whether overdetermination is a substantive causal claim or a placeholder for causal indeterminacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__overdetermined_composite_reading, 1800, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(duel_tr_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1820, 0.15).
narrative_ontology:measurement(duel_tr_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1840, 0.22).
narrative_ontology:measurement(duel_tr_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1860, 0.28).
narrative_ontology:measurement(duel_tr_t1870, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1870, 0.35).
narrative_ontology:measurement(duel_tr_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1880, 0.4).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1900, 0.4).

% Extraction over time
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1800, 0.2).
narrative_ontology:measurement(duel_be_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1820, 0.24).
narrative_ontology:measurement(duel_be_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1840, 0.3).
narrative_ontology:measurement(duel_be_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1860, 0.34).
narrative_ontology:measurement(duel_be_t1870, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1870, 0.4).
narrative_ontology:measurement(duel_be_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1880, 0.42).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1900, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1800, 0.25).
narrative_ontology:measurement(duel_su_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1820, 0.32).
narrative_ontology:measurement(duel_su_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1840, 0.4).
narrative_ontology:measurement(duel_su_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1860, 0.48).
narrative_ontology:measurement(duel_su_t1870, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1870, 0.55).
narrative_ontology:measurement(duel_su_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1880, 0.55).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1900, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, institutional_displacement_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the dueling_disappearance_mechanism kernel. contraction_reading attributes decline to dignity-culture displacement of honor-culture axioms (a cultural-normative mechanism); institutional_displacement_reading attributes decline to courts, banking, and libel law outcompeting dueling as dispute-resolution technology (an institutional-substitution mechanism); this overdetermined_composite_reading holds that these and Civil War trauma were jointly, non-separably sufficient, and authors a tangled_rope structure precisely because no single beneficiary or mechanism dominates. Each story carries its own ε, beneficiary/victim structure, and claimed_type; they are linked here rather than merged into one multi-parameter constraint, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
