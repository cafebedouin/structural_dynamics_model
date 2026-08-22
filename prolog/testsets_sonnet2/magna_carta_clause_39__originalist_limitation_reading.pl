% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__originalist_limitation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__originalist_limitation_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_clause_39__originalist_limitation_reading
 *   human_readable: Clause 39 as Bounded Remedy for Documented 1215 Royal Abuses (Originalist Reading)
 *   domain: constitutional_law/legal_history
 *
 * SUMMARY:
 *   This story authors the originalist reading of the Clause 39 kernel: the
 *   clause is a bounded, historically specific remedy negotiated between King
 *   John and his baronial tenants-in-chief in response to documented abuses
 *   (arbitrary disseisin, extrajudicial imprisonment, extortionate exchequer
 *   practices) that peaked between roughly 1207 and 1215. On this reading,
 *   the clause's operative content is fixed to that grievance set — it does
 *   not, by its own terms, establish a universal due-process guarantee or
 *   preserve a general feudal hierarchy; it simply removes specific latitude
 *   the Crown had exercised against a specific class of free men. John's
 *   repudiation within weeks (papal annulment, August 1215) and the
 *   subsequent reissues under Henry III's minority government (1216, 1217,
 *   1225) are read as the historical record of this narrow bargain being
 *   renegotiated and periodically reaffirmed among the same class of parties,
 *   not as evidence the clause meant something broader from the outset.
 *
 * KEY AGENTS:
 *   - baronial_signatories_1215: Primary beneficiary (powerful/constrained) — secured the textual remedy
 *   - crown_fiscal_prerogative: Primary target (institutional/constrained) — loses discretionary fiscal and judicial latitude specifically over the baronial class
 *   - royal_household_officials: Secondary target (powerful/constrained) — administrative machinery constrained
 *   - unfree_villein_population: Excluded — never covered by 'liber homo'
 *   - later_constitutional_interpreters: Excluded from THIS reading's scope claim — their broader readings are a different constraint
 *   - legal_historians: Analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__originalist_limitation_reading, 0.32).
domain_priors:suppression_score(magna_carta_clause_39__originalist_limitation_reading, 0.28).
domain_priors:theater_ratio(magna_carta_clause_39__originalist_limitation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__originalist_limitation_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__originalist_limitation_reading, "Clause 39 as Bounded Remedy for Documented 1215 Royal Abuses (Originalist Reading)").
narrative_ontology:topic_domain(magna_carta_clause_39__originalist_limitation_reading, "constitutional_law/legal_history").

domain_priors:requires_active_enforcement(magna_carta_clause_39__originalist_limitation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__originalist_limitation_reading, '3ed6f66e-a87b-4ebb-b063-576a45866e3b').
narrative_ontology:cs_kernel_codification('3ed6f66e-a87b-4ebb-b063-576a45866e3b', fixed_text).
narrative_ontology:cs_authority_grounding('3ed6f66e-a87b-4ebb-b063-576a45866e3b', lineage).
narrative_ontology:cs_interpretation_layer_present('3ed6f66e-a87b-4ebb-b063-576a45866e3b').
narrative_ontology:cs_reading_relation('3ed6f66e-a87b-4ebb-b063-576a45866e3b', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ed6f66e-a87b-4ebb-b063-576a45866e3b', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_axiom('3ed6f66e-a87b-4ebb-b063-576a45866e3b', foundational, clause_meaning_fixed_to_documented_1215_grievance_set).
narrative_ontology:cs_axiom_status(clause_meaning_fixed_to_documented_1215_grievance_set, holdable).
narrative_ontology:cs_axiom_grounding('3ed6f66e-a87b-4ebb-b063-576a45866e3b', clause_meaning_fixed_to_documented_1215_grievance_set, empirically_contingent).
narrative_ontology:cs_axiom('3ed6f66e-a87b-4ebb-b063-576a45866e3b', secondary, later_interpretive_expansion_is_a_distinct_constraint_not_original_content).
narrative_ontology:cs_axiom_status(later_interpretive_expansion_is_a_distinct_constraint_not_original_content, holdable).
narrative_ontology:cs_axiom_grounding('3ed6f66e-a87b-4ebb-b063-576a45866e3b', later_interpretive_expansion_is_a_distinct_constraint_not_original_content, conventional).
narrative_ontology:cs_reference_frame('3ed6f66e-a87b-4ebb-b063-576a45866e3b', documented_1215_baronial_grievance_remedy).
narrative_ontology:cs_drift_state('3ed6f66e-a87b-4ebb-b063-576a45866e3b', post_reissue_conciliar_confirmation_1225, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('3ed6f66e-a87b-4ebb-b063-576a45866e3b', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, baronial_signatories_1215).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, feudal_tenants_in_chief).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, crown_fiscal_prerogative).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, royal_household_officials).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__originalist_limitation_reading, textualist_constitutional_interpretation).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__originalist_limitation_reading, historically_bounded_grievance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The rebel barons who forced King John to seal the charter at Runnymede. They secured a specific textual guarantee that free men would not be arrested, imprisoned, or dispossessed except by lawful judgment of peers or the law of the land, as a direct remedy against John's documented practices of arbitrary disseisin, extrajudicial imprisonment, and fiscal extortion through the exchequer. Their exit option was armed rebellion, which they had already exercised; the charter was the negotiated alternative to continued civil war.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, baronial_signatories_1215, beneficiary,
    powerful, biographical, constrained, regional).

% The Crown's prior unchecked capacity to seize baronial lands, levy arbitrary fines and scutage, and imprison without process — used heavily by John to fund the Angevin wars and fill a depleted treasury. Clause 39 removes this specific latitude with respect to the baronial class. The Crown could still exit through renunciation (as John did within weeks via papal annulment), but reissue under Henry III and confirmation under Edward I foreclosed that route over time.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, crown_fiscal_prerogative, payer,
    institutional, generational, constrained, national).

% Sheriffs, justiciars, and exchequer officials who had executed John's extralegal seizures and imprisonments on his behalf. Clause 39 constrains their operating latitude by requiring judgment of peers or law of the land before acting against free men — a direct procedural check on the administrative machinery John's regime had relied upon.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, royal_household_officials, payer,
    powerful, biographical, constrained, national).

% The majority of the thirteenth-century population, legally unfree, who were never contemplated by the phrase 'liber homo' (free man) in Clause 39's text and had no voice in the Runnymede negotiations. Under the originalist reading their exclusion is not an oversight to be corrected by later interpretation but an accurate description of the clause's actual 1215 scope.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, unfree_villein_population, excluded,
    powerless, biographical, trapped, local).

% Seventeenth-century common lawyers (Coke), American founders, and modern courts who read Clause 39 as establishing due process and habeas corpus principles of universal application. The originalist reading treats their expansive readings as anachronistic accretions layered onto a text whose actual grievance-remedy was narrow and time-bound; their voice is structurally present in the historical record but excluded from THIS reading's account of what the clause itself meant and did in 1215.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, later_constitutional_interpreters, excluded,
    institutional, civilizational, analytical, global).

% Scholars who reconstruct the specific 1215 grievances (disseisin under John, arbitrary amercements, exchequer abuses documented in the Articles of the Barons) against which Clause 39 was drafted as remedy, and who trace how the clause's operative meaning shifted through successive reissues and later interpretive traditions.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__originalist_limitation_reading, baronial_signatories_1215).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__originalist_limitation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the specific 1215 crisis between King John and his rebellious tenants-in-chief by converting a set of documented, concrete grievances (arbitrary disseisin, extrajudicial imprisonment, exchequer extortion) into a negotiated textual constraint on royal action against the baronial class, averting continued civil war.
% TRANSFER_FUNCTION: Moves discretionary fiscal and judicial latitude away from the Crown and its household officials and back to the baronial class, in the specific form of requiring lawful judgment of peers or law of the land before seizure of person or property of free men.
% ABSENT_VOICES: The unfree villein population, who made up the majority of the population and were never covered by 'liber homo,' had no seat at Runnymede and are excluded from this reading's account by design, not oversight. Later interpreters who read universal rights into the text are also excluded here, since this reading is specifically about what the clause meant and did in its own documented context.
% DISAPPEARANCE_RATIONALE: Under the originalist reading, if Clause 39 had never been sealed, the specific 1215 crisis between John and the barons would likely have continued as civil war or ended in unconditional baronial defeat or unconditional royal capitulation; the negotiated middle settlement would not have existed. Whether the WORLD (in the broader civilizational sense) rearranges is contested precisely because that question depends on which reading of the clause's later effect one adopts — a dispute this reading brackets by keeping its claim confined to the 1215 remedy itself.
% FOUNDING_PROBLEM: King John's documented practice, especially after 1207, of arbitrary disseisin of baronial lands, imprisonment without judgment, and extortionate fiscal exactions (scutage, tallage, relief) to fund the failed Angevin campaigns in France, which had pushed the tenant-in-chief class to armed rebellion by 1215.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the baronial beneficiary class (drawing on the Articles of the Barons, pipe rolls, and chronicle accounts from Roger of Wendover and others) corroborate that John's specific fiscal and judicial abuses were the proximate cause and that those particular practices ceased with his death in 1216 and the subsequent reissues under conciliar government during Henry III's minority; the originalist reading holds that the founding problem, so specified, no longer exists, even though later readings treat the clause as addressing a continuing problem of arbitrary state power in general.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__originalist_limitation_reading, contested).
narrative_ontology:founding_problem_status(magna_carta_clause_39__originalist_limitation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__originalist_limitation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_clause_39__originalist_limitation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__originalist_limitation_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__originalist_limitation_reading_tests).
:- end_tests(magna_carta_clause_39__originalist_limitation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored as moderate (0.32) rather than high, because on this reading the clause extracts only the specific discretionary latitude John had exercised against the documented grievances — not a general transfer of sovereign power. Theater ratio spikes in 1216 (0.6) reflecting John's near-immediate repudiation, when the clause's textual existence outran its enforceability; it settles lower once conciliar reissues under Henry III restored operative force, though it drifts back upward toward 1225 as reissue becomes increasingly ceremonial reaffirmation of an already-settled baronial position rather than live renegotiation of a contested grievance. Suppression (0.28) is comparatively low: the constraint on the Crown was achieved through negotiated settlement backed by the credible threat of renewed rebellion, not through an ongoing coercive enforcement apparatus bearing on the baronial beneficiaries themselves.
 *
 * DIRECTIONALITY LOGIC:
 *   Baronial signatories sit near the beneficiary end: the constraint was drafted at their instigation and for their specific protection, and it removed real fiscal and judicial exposure they had experienced under John. The Crown's fiscal prerogative and royal household officials sit toward the target end: their prior discretionary latitude against the documented grievance set is what the clause removes. Because this reading limits the beneficiary class specifically to 'free men' as understood in 1215 — which functionally meant the baronial and freeholding class, not the unfree majority — the victim/beneficiary sets are narrower than either the feudal-hierarchy reading (which reads the same protections as extending through the whole free hierarchy) or the liberal reading (which reads them as universal).
 *
 * MANDATROPHY ANALYSIS:
 *   The originalist reading resists mandatrophy misclassification in a specific direction: it prevents treating the clause's LATER expansive function (habeas corpus doctrine, due process jurisprudence, American constitutional inheritance) as evidence about what the clause itself coordinated or extracted in 1215. Reading the clause's founding problem as dead (John's specific abuses ended with his death and the conciliar reissues) does not mean the clause's LATER incarnations are mandatrophic — those are different constraints in the kernel family, evaluated under their own readings, with different founding problems that may still be live. This story is confined to what the 1215 text did for the 1215 parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_scope_boundary_ambiguity,
    'Is the phrase ''liber homo'' in Clause 39 best read, even on originalist grounds, as bounded strictly to the baronial tenant-in-chief class who negotiated at Runnymede, or did it already extend informally to the broader class of free (non-villein) landholders in 1215 practice?',
    'Close reading of the Articles of the Barons alongside pipe roll and eyre records showing which classes of free men actually invoked Clause 39-type protections in the years immediately following 1215 and 1225 reissues.',
    'A broader documented 1215 beneficiary class would widen this reading''s own beneficiary set without converting it into the feudal_prerogative_reading or the liberal_due_process_reading, since it would remain bounded to 1215 evidence rather than extending to modern universal application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_scope_boundary_ambiguity, empirical, 'Whether the originalist beneficiary class is narrower or somewhat broader than the baronial signatories alone.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three kernel readings (originalist_limitation, feudal_prerogative, liberal_due_process) diverge — is it the identity of the beneficiary class, the temporal scope of the obligation, or the underlying theory of what kind of right the clause creates (concrete grievance-remedy vs. status-preserving procedural right vs. universal natural right)?',
    'Comparative analysis of all three sibling constraint stories'' beneficiary/victim declarations and axiom sets, cross-referenced against the historical record of how each interpretive tradition (Coke, American founders, modern comparative legal history) has actually cited the clause.',
    'Locating the disagreement precisely at the beneficiary-class and temporal-scope axes (rather than at competing empirical claims about 1215 events) would confirm this is a genuine committer/kernel structure — three parties reading the same stabilized text through different normative and interpretive commitments — rather than a resolvable factual dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the three kernel readings diverge on beneficiary class, temporal scope, or underlying rights theory.').

omega_variable(
    reissue_continuity_question,
    'Do the 1216, 1217, and 1225 reissues of the charter, under conciliar minority government rather than a defeated king, represent continuity of the same 1215 bargain or a structurally new negotiation whose founding problem differs from John''s original abuses?',
    'Comparison of textual variants across the reissues and analysis of who negotiated each reissue and against what contemporary grievances.',
    'If the reissues address a genuinely new founding problem (minority governance legitimacy, baronial consent to Henry III''s regency) rather than John''s original abuses, this reading''s claim that the founding problem is ''dead'' by 1216 requires qualification — the clause may have acquired a second, distinct 1216-1225 founding problem before its later medieval and early-modern reinterpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reissue_continuity_question, empirical, 'Whether the post-1215 reissues share or replace the original founding problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__originalist_limitation_reading, 1215, 1225).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1215, 0.2).
narrative_ontology:measurement(magn_tr_t1216, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1216, 0.6).
narrative_ontology:measurement(magn_tr_t1217, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1217, 0.35).
narrative_ontology:measurement(magn_tr_t1220, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1220, 0.4).
narrative_ontology:measurement(magn_tr_t1225, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1225, 0.45).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1215, 0.38).
narrative_ontology:measurement(magn_be_t1216, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1216, 0.1).
narrative_ontology:measurement(magn_be_t1217, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1217, 0.28).
narrative_ontology:measurement(magn_be_t1220, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1220, 0.3).
narrative_ontology:measurement(magn_be_t1225, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1225, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(magna_carta_clause_39__originalist_limitation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__feudal_prerogative_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the magna_carta_clause_39 kernel, decomposed per the ε-invariance principle because the natural-language label 'Clause 39' covers structurally distinct claims with different beneficiary sets, different temporal scopes, and different ε values. The originalist_limitation_reading authors the narrowest scope (ε=0.32, victims bounded to Crown fiscal prerogative and household officials as they stood in 1215, beneficiaries bounded to the baronial signatories). The feudal_prerogative_reading authors a broader within-hierarchy beneficiary class while still treating the clause as status-preserving rather than universal. The liberal_due_process_reading authors the broadest beneficiary class (all persons subject to arbitrary state power) and correspondingly higher extraction against state prerogative generally. All three share the same fixed text and lineage-grounded authority structure but diverge on reference frame and axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
