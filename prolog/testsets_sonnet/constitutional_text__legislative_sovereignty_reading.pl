% ============================================================================
% CONSTRAINT STORY: constitutional_text__legislative_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__legislative_sovereignty_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: constitutional_text__legislative_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty Reading of Constitutional Supremacy (Notwithstanding/Override Model)
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This story instantiates the legislative-sovereignty reading of the
 *   constitutional_text kernel: the constitution is read as establishing
 *   parliament, not courts, as the final arbiter of constitutional meaning,
 *   with judicial review reduced to an advisory or delayable function through
 *   notwithstanding clauses or simple legislative override. This is a
 *   genuinely distinct constraint from the judicial_supremacy_reading and
 *   popular_sovereignty_reading siblings — it has its own beneficiary
 *   structure (majoritarian coalitions), its own victim structure (minorities
 *   and unpopular claimants whose protection is only as durable as current
 *   majority sentiment), and its own persistence mechanism (electoral
 *   accountability rather than judicial enforcement). The three readings are
 *   not measured on a shared ε; each is authored as its own constraint per
 *   the ε-invariance principle, linked through the kernel network.
 *
 * KEY AGENTS:
 *   - legislative_majority: Primary beneficiary and agenda_setter (institutional/arbitrage) — controls override mechanism, no binding external check
 *   - constitutional_courts: Advisory observer, structurally excluded from final authority (institutional/constrained) — can delay but not block
 *   - constitutional_minorities: Primary structural payer (powerless/trapped) — protection is provisional on majority forbearance
 *   - unpopular_rights_claimants: Secondary payer (powerless/trapped) — rulings in their favor are the most frequent override targets
 *   - median_voter_coalition: Diffuse beneficiary (organized/constrained) — gains policy responsiveness
 *   - comparative_legal_scholars: Analytical observer (analytical/analytical) — cross-jurisdictional pattern tracking
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.42).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.38).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Parliamentary Sovereignty Reading of Constitutional Supremacy (Notwithstanding/Override Model)").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, 'f30ee64c-e4f7-4189-b15c-cb0ea7058b32').
narrative_ontology:cs_kernel_codification('f30ee64c-e4f7-4189-b15c-cb0ea7058b32', fixed_text).
narrative_ontology:cs_authority_grounding('f30ee64c-e4f7-4189-b15c-cb0ea7058b32', practice).
narrative_ontology:cs_interpretation_layer_present('f30ee64c-e4f7-4189-b15c-cb0ea7058b32').
narrative_ontology:cs_reading_relation('f30ee64c-e4f7-4189-b15c-cb0ea7058b32', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('f30ee64c-e4f7-4189-b15c-cb0ea7058b32', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('f30ee64c-e4f7-4189-b15c-cb0ea7058b32', foundational, elected_body_holds_ultimate_interpretive_authority).
narrative_ontology:cs_axiom_status(elected_body_holds_ultimate_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('f30ee64c-e4f7-4189-b15c-cb0ea7058b32', elected_body_holds_ultimate_interpretive_authority, conventional).
narrative_ontology:cs_axiom('f30ee64c-e4f7-4189-b15c-cb0ea7058b32', foundational, judicial_review_is_advisory_not_conclusive).
narrative_ontology:cs_axiom_status(judicial_review_is_advisory_not_conclusive, holdable).
narrative_ontology:cs_axiom_grounding('f30ee64c-e4f7-4189-b15c-cb0ea7058b32', judicial_review_is_advisory_not_conclusive, instrumental).
narrative_ontology:cs_reference_frame('f30ee64c-e4f7-4189-b15c-cb0ea7058b32', westminster_parliamentary_supremacy_tradition).
narrative_ontology:cs_drift_state('f30ee64c-e4f7-4189-b15c-cb0ea7058b32', contemporary_rights_litigation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f30ee64c-e4f7-4189-b15c-cb0ea7058b32', '').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, legislative_majority).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, median_voter_coalition).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, constitutional_minorities).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, unpopular_rights_claimants).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, democratic_self_correction_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the votes to invoke override or notwithstanding provisions whenever judicial review would otherwise block a statute. Frames this as democratic accountability: the elected body, not appointed judges, should have the final word on contested constitutional questions. Faces no binding external check beyond periodic elections.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, legislative_majority, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, legislative_majority, beneficiary).

% Reviews legislation and issues rulings on constitutional compatibility, but the ruling functions as advisory once the legislature can override it through a specified procedure. Courts retain reputational and persuasive authority but no final legal authority; can only delay, not block, sustained legislative will.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, constitutional_courts, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, constitutional_courts, excluded).

% Groups whose rights claims depend on judicial protection against majoritarian legislation — religious minorities, criminal defendants, out-groups without electoral weight. Under this reading, a court finding in their favor can be legislatively overridden, so their protection lasts only as long as it does not conflict with a determined majority. Cannot exit the jurisdiction's legislative authority; political mobilization is their only recourse and it is exactly the recourse the majority already controls.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, constitutional_minorities, payer,
    powerless, generational, trapped, national).

% Individuals asserting rights that are unpopular at a given political moment (speech, due process, association) find that a favorable constitutional ruling is provisional pending legislative override. Their remedy is subject to being legislated away by the same body whose action prompted the litigation.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, unpopular_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Broad electorate whose preferences the legislature is structurally positioned to enact without judicial obstruction. Gains policy responsiveness — laws reflecting current majority preference are not frozen by prior constitutional interpretation they may no longer endorse.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, median_voter_coalition, beneficiary,
    organized, biographical, constrained, national).

% Study how notwithstanding-clause systems (Canada, Israel's override mechanisms, UK parliamentary sovereignty tradition) perform relative to strong judicial review systems, comparing rights outcomes and majoritarian responsiveness across jurisdictions.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the question of final interpretive authority by locating it in the body most directly and frequently accountable to the electorate, avoiding rule by an unelected judiciary on contested moral and policy questions where reasonable disagreement exists.
% TRANSFER_FUNCTION: Moves final say over the meaning and durability of constitutional rights from courts to legislative majorities; in practice this transfers the marginal protection of minority and unpopular claims to majoritarian political outcomes.
% ABSENT_VOICES: Constitutional minorities and unpopular rights claimants are formally represented by elected officials but have no institutional voice that survives a hostile legislative majority; their objection — that rights protected only at majority sufferance are not rights in the relevant sense — is heard in academic and judicial commentary but has no binding forum.
% DISAPPEARANCE_RATIONALE: If legislative override authority were removed and judicial rulings became final, the balance of power would shift substantially: courts would gain the practical capacity to entrench rights against subsequent majorities, legislatures would lose the ability to correct judicial interpretations they consider mistaken, and the political calculus around contested rights litigation would change on both sides.
% FOUNDING_PROBLEM: Historically framed as solving the 'counter-majoritarian difficulty' — the concern that unelected judges wielding final constitutional authority lack democratic legitimacy to override the will of elected representatives, particularly on contested value questions without a single correct legal answer.
% FOUNDING_PROBLEM_CORROBORATION: Legislative sovereignty's own architects and sitting parliamentarians attest the problem (judicial overreach into contestable policy) remains live. Independent comparative-law scholarship and minority-rights advocacy organizations, outside the beneficiary coalition, corroborate that override mechanisms have in practice been invoked disproportionately against rulings protecting minority and unpopular claimants rather than against genuinely indeterminate policy questions — suggesting the mechanism's use has drifted from its stated justification.
narrative_ontology:disappearance_verdict(constitutional_text__legislative_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__legislative_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__legislative_sovereignty_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__legislative_sovereignty_reading_tests).
:- end_tests(constitutional_text__legislative_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than severe because the mechanism genuinely does solve a real coordination problem — democratic accountability for constitutional interpretation — for the median voter and majority coalition; it is not pure extraction dressed as coordination. But it is not zero because a structurally identifiable class (constitutional minorities, unpopular claimants) bears the cost through the same mechanism that delivers majoritarian responsiveness to everyone else, which is the tangled-rope signature: genuine coordination function plus asymmetric extraction through the identical structure. Suppression is moderate (0.38) because override requires active legislative will and periodic re-invocation — it is not passive; theater is comparatively low (0.28) because the override mechanism, when invoked, has real legal effect (it is not merely symbolic advisory theater; it genuinely nullifies judicial protection). The measurement series shows both extractiveness and suppression climbing modestly over the interval, consistent with the founding_problem_corroboration finding that override use has drifted from correcting indeterminate policy questions toward overriding minority-protective rulings specifically.
 *
 * PERSPECTIVAL GAP:
 *   From the legislative_majority's seat, this reading is democratic self-governance correcting judicial overreach — a rope, arguably even approaching mountain-like inevitability in majoritarian systems. From the constitutional_minorities' seat, the identical structure is a mechanism whose entire function is removing the one check that existed on majority action against them — closer to snare. The engine computes both from the same structural data; the divergence is the finding, not an error to reconcile. The claimed_type here (tangled_rope) sits between these two seat-level readings deliberately, reflecting that the constraint genuinely has both a real coordination function and a real extraction channel operating through the same override mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative_majority sits at the beneficiary end: it sets the rule, invokes it at will, and bears no cost from its own action. The median_voter_coalition is a diffuse beneficiary — responsive policy without judicial obstruction — but with more constrained exit than the legislature itself (voters cannot simply leave the jurisdiction to escape a policy they dislike, though they can vote). Constitutional_minorities and unpopular_rights_claimants sit at the target end: trapped exit options (no meaningful jurisdictional exit, no electoral leverage proportional to their numbers), and the same override mechanism that serves majoritarian responsiveness for everyone else is precisely the mechanism that removes their protection. This is the asymmetric extraction the tangled_rope classification requires: one coordination structure, two structurally opposed outcomes depending on which seat you occupy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (counter-majoritarian difficulty) has genuine ongoing relevance in cases of indeterminate policy — this prevents dismissing the whole arrangement as pure extraction. But the founding_problem_status is authored as contested precisely because independent corroboration (outside the legislative beneficiary coalition) indicates the override mechanism's actual invocation pattern has drifted toward targeting minority-protective rulings specifically, rather than genuinely indeterminate value questions. This is exactly the mandatrophy signature: a mandate (correcting judicial overreach on contestable questions) whose exercise has partially decoupled from its stated justification while retaining the justification's rhetorical cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    override_frequency_vs_indeterminacy,
    'Is the notwithstanding/override mechanism actually invoked predominantly on genuinely indeterminate constitutional questions (its stated justification), or predominantly against rulings protecting minorities and unpopular claimants?',
    'Empirical audit of override/notwithstanding invocations across jurisdictions using this model (Canada''s Section 33, comparable override provisions), coded by whether the underlying judicial ruling concerned indeterminate policy versus minority rights protection.',
    'If invocation is concentrated on minority-protective rulings, the founding_problem_status should be read as substantially dead relative to its stated justification, strengthening the snare-ward reading from the victim seat. If invocation is genuinely spread across indeterminate policy questions, the tangled_rope classification''s coordination component is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_frequency_vs_indeterminacy, empirical, 'Whether override use tracks its stated justification or has drifted toward targeting minority protections.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the constitutional text''s designation of legislative finality itself unambiguous, or does the text''s own wording (e.g., open-ended rights language paired with an override clause) leave the locus of ultimate authority genuinely underdetermined, such that the legislative_sovereignty_reading is one plausible construction among several rather than the text''s plain meaning?',
    'Close textual and drafting-history analysis of the specific override/notwithstanding provision''s placement, scope limits, and legislative debate record at time of adoption; comparison with sibling jurisdictions'' drafting history.',
    'If the text is genuinely ambiguous between readings, this reading''s legitimacy rests more heavily on subsequent institutional practice and less on textual compulsion, which would elevate the popular_sovereignty_reading''s claim that authority ultimately resides in constituent processes not yet exercised. If the text clearly compels legislative finality, this reading''s textual grounding is stronger than its practical-outcomes critics suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the constitutional text unambiguously supports this reading or whether the reading is one contestable construction among plausible alternatives.').

omega_variable(
    minority_protection_natural_vs_constructed,
    'Is the vulnerability of constitutional_minorities and unpopular_rights_claimants under this reading a natural consequence of any majoritarian system, or a constructed feature specifically attributable to the choice to make judicial review overridable rather than final?',
    'Comparative outcome analysis: do minority rights outcomes differ measurably between jurisdictions with override mechanisms and matched jurisdictions with strong judicial review, controlling for other institutional and cultural variables?',
    'If outcomes are substantially similar regardless of override availability, the mechanism''s contribution to minority vulnerability is smaller than the extractiveness score suggests. If outcomes diverge substantially and predictably with override availability, it corroborates treating the override mechanism itself as the operative extraction channel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_natural_vs_constructed, empirical, 'Whether override-specific institutional design measurably worsens minority rights outcomes relative to comparable systems without it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__legislative_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t8, constitutional_text__legislative_sovereignty_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(cons_tr_t16, constitutional_text__legislative_sovereignty_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(cons_tr_t24, constitutional_text__legislative_sovereignty_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(cons_tr_t32, constitutional_text__legislative_sovereignty_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__legislative_sovereignty_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cons_be_t8, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(cons_be_t16, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(cons_be_t24, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(cons_be_t32, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(cons_be_t40, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cons_su_t8, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(cons_su_t16, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 16, 0.31).
narrative_ontology:measurement(cons_su_t24, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(cons_su_t32, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(cons_su_t40, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text__legislative_sovereignty_reading, 0.1).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'constitutional supremacy' per the ε-invariance principle. judicial_supremacy_reading assigns final interpretive authority to courts (ε and beneficiary/victim structure differ: judges and rights claimants benefit from entrenchment, legislative majorities bear the cost of being overridden). popular_sovereignty_reading locates final authority in extraordinary constituent processes rather than any ordinary institutional body, producing yet a third distinct ε and victim structure (routine legislative and judicial action alike become provisional pending constituent revision). All three are linked here rather than merged because measuring 'who has final constitutional authority' one way (legislative texts and override clauses) versus another way (judicial invalidation power) versus a third way (constituent/amendment processes) yields genuinely different extraction profiles, not different measurements of one profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
