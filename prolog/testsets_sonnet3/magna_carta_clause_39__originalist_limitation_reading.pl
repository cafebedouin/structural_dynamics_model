% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__originalist_limitation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Clause 39 as Narrow Remedy for Documented 1215 Royal Abuses (Originalist Reading)
 *   domain: constitutional_law/legal_history
 *
 * SUMMARY:
 *   This story instantiates the originalist limitation reading of the Clause
 *   39 kernel: the clause is understood strictly as a negotiated remedy for
 *   the specific, documented abuses of King John against his baronial
 *   tenants-in-chief in the years leading to 1215 — arbitrary disseisin,
 *   imprisonment without lawful judgment, exile without process — and nothing
 *   more. On this reading the victim class is bounded to the barons who
 *   bargained for the text; freemen and the unfree peasantry are outside the
 *   constraint's original scope, and any later reading that extends Clause 39
 *   into a general due-process guarantee is, from this seat, an anachronistic
 *   expansion of the original bargain rather than a discovery of its true
 *   meaning. This is a distinct constraint from the
 *   liberal_due_process_reading (which reads the same text as establishing
 *   universal rights against arbitrary state power) and from the
 *   feudal_prerogative_reading (which reads it as preserving narrow
 *   procedural rights within, not against, the hierarchical order). Each
 *   reading has its own ε, its own beneficiary/victim structure, and its own
 *   classification; they are linked, not merged.
 *
 * KEY AGENTS:
 *   - baronial_signatories: Primary beneficiary (powerful/constrained) — negotiated the specific remedy
 *   - king_john_and_crown_administration: Agenda-setter (institutional/constrained) — bound its own documented prerogative abuses
 *   - freemen_outside_baronial_class: Payer/bystander (powerless/trapped) — outside the documented grievance set, unaddressed
 *   - villeins_and_unfree_peasantry: Excluded (powerless/trapped) — never part of the negotiation
 *   - later_common_law_courts: Analytical observer (institutional/analytical) — later doctrinal expansion the originalist reading treats as anachronistic
 *   - constitutional_historians: Analytical observer (analytical/analytical) — reconstructs the bounded 1215 context
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
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__originalist_limitation_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__originalist_limitation_reading, "Clause 39 as Narrow Remedy for Documented 1215 Royal Abuses (Originalist Reading)").
narrative_ontology:topic_domain(magna_carta_clause_39__originalist_limitation_reading, "constitutional_law/legal_history").

domain_priors:requires_active_enforcement(magna_carta_clause_39__originalist_limitation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__originalist_limitation_reading, '61a02370-d189-44be-9116-c7eb30911960').
narrative_ontology:cs_kernel_codification('61a02370-d189-44be-9116-c7eb30911960', fixed_text).
narrative_ontology:cs_authority_grounding('61a02370-d189-44be-9116-c7eb30911960', lineage).
narrative_ontology:cs_interpretation_layer_present('61a02370-d189-44be-9116-c7eb30911960').
narrative_ontology:cs_reading_relation('61a02370-d189-44be-9116-c7eb30911960', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('61a02370-d189-44be-9116-c7eb30911960', magna_carta_clause_39__feudal_prerogative_reading, influences).
narrative_ontology:cs_axiom('61a02370-d189-44be-9116-c7eb30911960', foundational, clause_scope_bounded_by_documented_1215_grievances).
narrative_ontology:cs_axiom_status(clause_scope_bounded_by_documented_1215_grievances, holdable).
narrative_ontology:cs_axiom_grounding('61a02370-d189-44be-9116-c7eb30911960', clause_scope_bounded_by_documented_1215_grievances, empirically_contingent).
narrative_ontology:cs_axiom('61a02370-d189-44be-9116-c7eb30911960', foundational, textual_meaning_fixed_at_negotiation_not_amenable_to_later_generalization).
narrative_ontology:cs_axiom_status(textual_meaning_fixed_at_negotiation_not_amenable_to_later_generalization, holdable).
narrative_ontology:cs_axiom_grounding('61a02370-d189-44be-9116-c7eb30911960', textual_meaning_fixed_at_negotiation_not_amenable_to_later_generalization, conventional).
narrative_ontology:cs_reference_frame('61a02370-d189-44be-9116-c7eb30911960', documented_1215_baronial_grievance_settlement).
narrative_ontology:cs_drift_state('61a02370-d189-44be-9116-c7eb30911960', post_coke_common_law_revival, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('61a02370-d189-44be-9116-c7eb30911960', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, baronial_signatories).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, king_john_successors_seeking_stable_settlement).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, freemen_outside_baronial_class).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, crown_prerogative_over_disseisin).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The rebel barons who forced King John to seal the charter after specific grievances — arbitrary disseisin, imprisonment without judgment, extortionate scutage. On this reading, Clause 39 is a negotiated settlement addressed to their documented injuries, not a general theory of rights. They receive a concrete, enforceable guarantee against the specific abuses they experienced under John's reign.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, baronial_signatories, beneficiary,
    powerful, biographical, constrained, national).

% The Crown, under military and political pressure, agrees to bind its own future exercises of specific prerogatives (disseisin, imprisonment, exile without lawful judgment of peers or the law of the land) as the price of ending the baronial revolt. The King retains all prerogatives not enumerated as abused; this reading treats the clause as a targeted concession, not a surrender of general sovereign power.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, king_john_and_crown_administration, agenda_setter,
    institutional, biographical, constrained, national).

% Free tenants, lesser freeholders, and townspeople who were not party to the 1215 negotiation and whose grievances (if any) are not the documented context the clause addresses. On the originalist reading their situation is unchanged by Clause 39 — the clause was never drafted with their abuses in view, so any protection they later claim under it is an extension beyond the constraint's actual scope, leaving their exposure to non-baronial forms of arbitrary treatment uncorrected.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, freemen_outside_baronial_class, payer,
    powerless, generational, trapped, national).

% The unfree majority of the population, excluded from the charter's protections entirely under thirteenth-century legal status categories. They are not part of the 1215 grievance set and this reading does not claim otherwise; their total absence from the negotiating table is precisely what the originalist reading treats as outside the constraint's scope rather than as an oversight to be corrected by later interpretation.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, villeins_and_unfree_peasantry, excluded,
    powerless, generational, trapped, local).

% Judges and jurists from the seventeenth century onward (Coke and successors) who read Clause 39 as the seed of due process. The originalist reading treats this line of interpretation as an anachronistic expansion — a later constitutional mythology grafted onto a narrow feudal settlement — and observes but does not endorse the doctrinal accretion.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, later_common_law_courts, observer,
    institutional, civilizational, analytical, national).

% Scholars who reconstruct the 1215 context — the specific abuses, the specific parties, the specific bargaining — to test how far later readings of Clause 39 track or depart from what was actually negotiated. They corroborate or challenge the originalist reading's claim that the clause's scope is bounded by documented grievances.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ends an active baronial revolt by giving the rebel barons an enforceable, specific commitment from the Crown not to repeat the particular abuses (arbitrary disseisin, imprisonment, exile) that provoked the rebellion — a targeted peace settlement, not a general charter of liberties.
% TRANSFER_FUNCTION: Moves a specific set of prerogative powers (summary disseisin, imprisonment without judgment) from unconstrained royal discretion to a judgment-of-peers-or-law-of-the-land requirement, but only as against the barons and only for the abuses actually documented in the 1215 dispute; no transfer occurs for classes or abuses outside that documented set.
% ABSENT_VOICES: Freemen below baronial rank and the unfree peasantry were not party to the 1215 negotiation and are not addressed by this reading's account of the clause's scope; later interpreters who read universal protection into the text are, on this reading, importing a concern the original parties did not bargain over.
% DISAPPEARANCE_RATIONALE: If the originalist reading's narrow scope were the only operative reading, the disappearance of Clause 39 today would rearrange almost nothing for most modern legal subjects since the reading denies the clause ever generalized beyond the baronial settlement; but constitutional historians and living-tradition courts dispute this, since due-process doctrine built on the clause's later readings does structure modern arrangements. The verdict is contested precisely because the originalist reading's scope claim is itself the site of the kernel dispute.
% FOUNDING_PROBLEM: King John's practice of seizing baronial land, imprisoning tenants-in-chief, and exiling opponents without any judicial process, driven by fiscal desperation and personal rule, provoked an armed baronial coalition that needed a negotiated, enforceable check on those specific practices to stand down.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside the baronial beneficiary class (e.g. the documentary record of the 1215 negotiations, the Articles of the Barons, and modern historiographic reconstruction of King John's specific administrative abuses) corroborate that the founding problem — those particular royal practices under that particular king — no longer exists in any form; the barons themselves as a political class are also long extinct, so no living beneficiary group survives to self-report on the clause's continuing necessity.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__originalist_limitation_reading, contested).
narrative_ontology:founding_problem_status(magna_carta_clause_39__originalist_limitation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__originalist_limitation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored moderate (0.32) because on this reading the constraint's coercive bite is genuinely narrow — it binds the Crown only against a specific, enumerated set of abuses experienced by a specific class, not a general extraction machine. Suppression is moderate-low (0.28): enforcement (via the security clause and the Council of Twenty-Five) was real but scoped to the same narrow set of grievances. Theater ratio is authored higher (0.45) because a substantial portion of the clause's later invocation — especially post-1216 reissues under Henry III with the security clause stripped — is performative continuity rather than functioning remedy; the measurement series shows a spike around 1216 (John's death, first reissue, security clause dropped) reflecting a shift toward symbolic reaffirmation over enforced remedy. Accessibility collapse (0.4) and resistance (0.35) are moderate: alternatives to the charter (continued civil war, unilateral royal restoration) persisted as live options throughout the interval, and baronial resistance to backsliding was active but intermittent.
 *
 * PERSPECTIVAL GAP:
 *   The baronial seat and the Crown seat compute differently: from the barons' position, Clause 39 is a genuine, functioning coordination mechanism (peace in exchange for enforceable limits); from the Crown's position under John and briefly under Henry III's minority, the same clause is an imposed constraint whose active enforcement (Council of Twenty-Five, security clause) it worked to erode as soon as political conditions allowed — hence the 1216 reissue stripping enforcement mechanisms. The originalist reading's central analytical move is to hold the SCOPE narrow while still registering this real, if bounded, tangled-rope structure between the two negotiating parties.
 *
 * DIRECTIONALITY LOGIC:
 *   Baronial signatories are the structural beneficiaries: they extracted a specific, enforceable concession from the Crown and are the low-d seat. The Crown/King John seat carries a constrained but not fully victimized directionality — it bears the cost of the concession but retains all unenumerated prerogatives, so its d sits closer to symmetric than to full-target. Freemen outside the baronial class and the unfree peasantry are NOT declared as high-d victims of Clause 39 itself on this reading — their exclusion is a scope limitation, not an extraction the clause performs against them, and this is exactly the structural delta between this reading and the liberal_due_process_reading. Where 'crown_prerogative_over_disseisin' is listed as a victim it names the institutional prerogative curtailed, not a suffering agent, and constrains only the enumerated practices.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (John's specific abuses) is genealogically dead — no living King John, no living baronial class experiencing those specific abuses — yet the text persists and is invoked continuously in later constitutional traditions for purposes the originalist reading holds were never part of the 1215 bargain. This is precisely a mandatrophy signature: an arrangement whose narrow founding purpose has ended while the arrangement (or at least its textual vessel) is repeatedly reactivated for new purposes. The originalist reading resists letting the clause's later career retroactively rewrite its original, bounded coordination function — it does not deny the later doctrinal tradition exists, only that Clause 39 ITSELF (as opposed to what jurists later built on it) extends that far.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    documented_grievance_boundary_stability,
    'Is the set of ''documented 1215 abuses'' a stable, closed historical fact, or is it itself a retrospective construction shaped by which sources survived and which later interpreters chose to canonize as ''the'' grievances?',
    'Comparative analysis of the Articles of the Barons, the 1215 charter text, and independent chronicle sources (e.g. the Barnwell chronicler) to establish how contested the grievance inventory was even among contemporaries.',
    'If the grievance set is itself contested or was already broader in contemporary understanding than modern originalist reconstructions assume, the originalist reading''s claimed narrow scope may itself be a later methodological artifact rather than a recovery of 1215 intent — narrowing the gap with the liberal_due_process_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documented_grievance_boundary_stability, conceptual, 'Whether the ''documented abuses'' boundary is a stable historical fact or a retrospective interpretive construction.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three kernel readings (originalist_limitation, liberal_due_process, feudal_prerogative) diverge structurally — is it the identity of the victim class, the scope of ''law of the land,'' or the temporal boundedness of the commitment?',
    'This is not resolvable by new evidence; it is a framing question about which interpretive community''s criteria for fidelity to a text govern constitutional meaning-making over an 800-year span.',
    'The originalist reading''s central structural claim is that the clause''s victim set and scope are BOUNDED (to barons, to documented abuses); the liberal_due_process_reading''s core structural claim is that ''law of the land'' functions as an open, generalizable standard; the feudal_prerogative_reading''s core claim is that the beneficiaries are bounded but the mechanism operates WITHIN rather than against hierarchy. All three readings can be held by different living legal communities simultaneously (courts, historians, political theorists) — none is forced to yield by the others'' existence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'The located structural disagreement between the three sibling readings of the Clause 39 kernel.').

omega_variable(
    enforcement_mechanism_originalist_scope,
    'Does the Council of Twenty-Five''s enforcement mechanism (security clause, distraint on royal property) belong to the originalist reading''s bounded scope, or does its removal in the 1216/1217 reissues mean the originalist reading should treat post-1216 Clause 39 as a structurally different, weaker constraint?',
    'Textual comparison of the 1215, 1216, 1217, and 1225 charter recensions to trace exactly which enforcement language persists and which is dropped.',
    'If enforcement is dropped after 1216, the originalist reading may need to further decompose into a pre-1216 (enforced) and post-1216 (unenforced, more theater-heavy) sub-constraint, consistent with the ε-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_originalist_scope, empirical, 'Whether enforcement mechanism removal after 1216 constitutes a distinct constraint requiring further decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__originalist_limitation_reading, 1215, 1225).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1215, 0.2).
narrative_ontology:measurement(magn_tr_t1216, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1216, 0.5).
narrative_ontology:measurement(magn_tr_t1217, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1217, 0.35).
narrative_ontology:measurement(magn_tr_t1220, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1220, 0.4).
narrative_ontology:measurement(magn_tr_t1225, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1225, 0.45).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1215, 0.35).
narrative_ontology:measurement(magn_be_t1216, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1216, 0.4).
narrative_ontology:measurement(magn_be_t1217, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1217, 0.3).
narrative_ontology:measurement(magn_be_t1220, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1220, 0.28).
narrative_ontology:measurement(magn_be_t1225, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1225, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(magna_carta_clause_39__originalist_limitation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__originalist_limitation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_clause_39__originalist_limitation_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__feudal_prerogative_reading).

% DUAL FORMULATION NOTE:
% Three sibling readings of the magna_carta_clause_39 kernel: this story (originalist_limitation_reading, narrowest scope, moderate ε, tangled_rope between barons and Crown), liberal_due_process_reading (widest scope, treats 'law of the land' as a generalizable universal-rights standard, expected much higher ε against a much broader victim set across centuries), and feudal_prerogative_reading (narrow beneficiary class but reads the mechanism as operating within rather than against hierarchical order, more rope-flavored). Each carries its own ε and classification; they are linked here rather than merged into one observer-relative story, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
