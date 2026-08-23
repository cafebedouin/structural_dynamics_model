% ============================================================================
% CONSTRAINT STORY: marriage_authority__judicial_harmonization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__judicial_harmonization_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: marriage_authority__judicial_harmonization_reading
 *   human_readable: Judicial Harmonization of Marriage Authority via Constitutional Floor
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   In jurisdictions with plural personal law systems (e.g., India, Israel,
 *   Lebanon), marriage authority traditionally resides in community-specific
 *   religious codes. Over the past four decades, supreme courts have
 *   incrementally imposed a constitutional floor — minimum equality, due
 *   process, and non-discrimination standards — across all personal law codes
 *   through case-by-case review, without waiting for legislative enactment of
 *   a Uniform Civil Code (UCC). This constraint story captures the judicial
 *   harmonization pathway as a distinct reading of the marriage_authority
 *   kernel: the claim that constitutional interpretation by the apex court
 *   can and should gradually harmonize marriage law toward equality, treating
 *   UCC legislation as the eventual sunset but not the prerequisite. The
 *   constraint is claimed as a scaffold because its justification is
 *   explicitly transitional — the court's expanded role is warranted only
 *   until the legislature fulfills its UCC mandate. Yet the transition has
 *   lasted decades with no UCC in sight, raising mandatrophy questions.
 *
 * KEY AGENTS:
 *   - supreme_judiciary: Primary agenda_setter (institutional/analytical) — imposes constitutional floor via precedent, claims transitional mandate
 *   - communal_personal_law_boards: Primary payers (organized/powerful) — lose interpretive monopoly over marriage norms, face judicial override
 *   - gender_rights_litigants: Beneficiaries (moderate/constrained) — gain constitutional protections within personal law, but remain dependent on case-by-case litigation
 *   - traditionalist_litigants: Payers (moderate/constrained) — experience erosion of communal autonomy, framed as religious freedom violation
 *   - legislature: Excluded (institutional/biographical) — UCC mandate unfulfilled, judicial action fills vacuum legislature avoids
 *   - constitutional_equality_doctrine: Vindicated proposition (non-agent) — gains operational force through judicial harmonization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, 0.42).
domain_priors:suppression_score(marriage_authority__judicial_harmonization_reading, 0.35).
domain_priors:theater_ratio(marriage_authority__judicial_harmonization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__judicial_harmonization_reading, scaffold).
narrative_ontology:human_readable(marriage_authority__judicial_harmonization_reading, "Judicial Harmonization of Marriage Authority via Constitutional Floor").
narrative_ontology:topic_domain(marriage_authority__judicial_harmonization_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__judicial_harmonization_reading).
narrative_ontology:has_sunset_clause(marriage_authority__judicial_harmonization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__judicial_harmonization_reading, '9dfb1879-4d0c-4c86-a491-3023e112871a').
narrative_ontology:cs_kernel_codification('9dfb1879-4d0c-4c86-a491-3023e112871a', fixed_text).
narrative_ontology:cs_authority_grounding('9dfb1879-4d0c-4c86-a491-3023e112871a', lineage).
narrative_ontology:cs_interpretation_layer_present('9dfb1879-4d0c-4c86-a491-3023e112871a').
narrative_ontology:cs_reading_relation('9dfb1879-4d0c-4c86-a491-3023e112871a', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('9dfb1879-4d0c-4c86-a491-3023e112871a', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('9dfb1879-4d0c-4c86-a491-3023e112871a', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('9dfb1879-4d0c-4c86-a491-3023e112871a', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_axiom('9dfb1879-4d0c-4c86-a491-3023e112871a', foundational, constitutional_floor_supersedes_personal_law).
narrative_ontology:cs_axiom_status(constitutional_floor_supersedes_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('9dfb1879-4d0c-4c86-a491-3023e112871a', constitutional_floor_supersedes_personal_law, conventional).
narrative_ontology:cs_axiom('9dfb1879-4d0c-4c86-a491-3023e112871a', foundational, judicial_harmonization_is_transitional_pending_ucc).
narrative_ontology:cs_axiom_status(judicial_harmonization_is_transitional_pending_ucc, holdable).
narrative_ontology:cs_axiom_grounding('9dfb1879-4d0c-4c86-a491-3023e112871a', judicial_harmonization_is_transitional_pending_ucc, conventional).
narrative_ontology:cs_reference_frame('9dfb1879-4d0c-4c86-a491-3023e112871a', constitutional_directive_principle_ucc).
narrative_ontology:cs_drift_state('9dfb1879-4d0c-4c86-a491-3023e112871a', contemporary_judicial_harmonization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9dfb1879-4d0c-4c86-a491-3023e112871a', '').
narrative_ontology:cs_kernel_id(marriage_authority__judicial_harmonization_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, supreme_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, gender_rights_litigants).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, constitutional_equality_doctrine).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, communal_personal_law_boards).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, traditionalist_litigants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, gender_rights_litigants).
narrative_ontology:constraint_vindicates(marriage_authority__judicial_harmonization_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__judicial_harmonization_reading, gradual_harmonization_principle).
narrative_ontology:constraint_vindicates(marriage_authority__judicial_harmonization_reading, judicial_avoidance_of_legislative_vacuum).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Imposes constitutional floor on marriage law through case-by-case precedent. Claims transitional mandate pending legislative UCC. Gains institutional authority, legitimacy capital, and agenda-setting power over family law. Faces no electoral accountability for marriage law outcomes. Can expand or contract the floor through doctrinal innovation.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, supreme_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Historically held interpretive monopoly over community marriage norms. Now face judicial override when norms conflict with constitutional floor. Lose authority incrementally through precedent. Can lobby legislature, issue fatwas/guidance, or seek constitutional amendments — all politically costly. Their exit is constrained by state recognition requirements and community legitimacy needs.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, communal_personal_law_boards, payer,
    organized, generational, constrained, national).

% Gain constitutional protections (equality, maintenance, custody, divorce rights) within personal law through strategic litigation. Benefits are real but case-dependent and reversible with judicial turnover. Bear litigation costs and uncertainty. No guaranteed systemic reform — each case re-litigates the floor. Exit options constrained by lack of alternative forums and dependence on judicial goodwill.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, gender_rights_litigants, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__judicial_harmonization_reading, gender_rights_litigants, payer).

% Experience erosion of communal autonomy in marriage — e.g., unilateral divorce restrictions, maintenance obligations, gender-equal inheritance. Frame losses as religious freedom violations. Bear compliance costs and symbolic injury. Exit options limited: can seek sympathetic forums (forum shopping increasingly restricted), appeal to legislature, or accept judicial rulings.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, traditionalist_litigants, payer,
    moderate, biographical, constrained, local).

% Constitutionally mandated to enact UCC but has not done so for decades. Judicial harmonization fills the vacuum, reducing political pressure for legislation. Could reclaim authority by enacting UCC but faces electoral risks from communal constituencies. Exit is mobile — can legislate at any time — but political incentives favor stasis.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, legislature, excluded,
    institutional, biographical, mobile, national).

% The doctrinal commitment to equality and non-discrimination as constitutional lodestars. Gains operational force and precedential depth through judicial harmonization. Does not collect rents or bear costs — a vindicated proposition, not an agent. Its 'exit' is conceptual: the doctrine persists or evolves independently of any particular institutional instantiation.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, constitutional_equality_doctrine, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_authority__judicial_harmonization_reading, constitutional_equality_doctrine).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__judicial_harmonization_reading, supreme_judiciary).
narrative_ontology:fixing_cost_class(marriage_authority__judicial_harmonization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable constitutional floor across fragmented personal law codes, preventing race-to-the-bottom in rights protections and reducing forum shopping. Solves the collective-action problem where no single community volunteers to reform its marriage law first, and the legislature avoids the political cost of UCC.
% TRANSFER_FUNCTION: Moves marriage law interpretive authority from communal_personal_law_boards and legislature to the supreme_judiciary. Transfers compliance costs and symbolic losses to traditionalist_litigants and communal boards. Transfers rights protections to gender_rights_litigants (contingent, case-by-case). The judiciary collects institutional authority and legitimacy capital as the gain_flow recipient.
% ABSENT_VOICES: Minority sub-communities within major personal law systems (e.g., reformist Muslims, progressive Christians, LGBTQ+ persons within traditional communities) who would challenge both communal boards' conservatism and judicial harmonization's majoritarian constitutionalism. They are excluded because neither communal nor judicial forums structurally represent intra-community dissent.
% DISAPPEARANCE_RATIONALE: If judicial harmonization vanished overnight, communal boards would regain full interpretive authority, gender_rights_litigants would lose constitutional floor protections, and the legislature would face renewed pressure for UCC. The personal law system would revert to pre-harmonization fragmentation with divergent rights outcomes across communities.
% FOUNDING_PROBLEM: Legislative vacuum on Uniform Civil Code: the constitution mandated UCC as a directive principle but no legislature enacted it for decades, leaving marriage law fragmented across personal codes with unequal gender outcomes. The judiciary stepped in to impose a constitutional floor as interim measure.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary attests the problem remains live (ongoing inequality in personal laws). Communal boards attest the problem is manufactured (judiciary creates the vacuum it then fills). Gender rights organizations attest the problem is real but the judicial solution is incomplete (case-by-case leaves gaps). Independent constitutional scholars outside the benefiting parties corroborate the legislative vacuum but dispute whether judicial harmonization is the constitutionally proper remedy.
narrative_ontology:disappearance_verdict(marriage_authority__judicial_harmonization_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__judicial_harmonization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__judicial_harmonization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(marriage_authority__judicial_harmonization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__judicial_harmonization_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__judicial_harmonization_reading_tests).
:- end_tests(marriage_authority__judicial_harmonization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the judiciary's capture of marriage authority allocation — a function historically held by communities and legislature — without bearing electoral accountability. The extraction is moderate because the court provides genuine coordination: a predictable constitutional floor reduces forum shopping, protects vulnerable parties, and prevents race-to-the-bottom in personal law competition. Suppression (0.35) is modest because communal boards retain substantial interpretive space above the floor; exit options exist (legislative override, constitutional amendment) but are politically costly. Theater (0.28) rises over time as the 'transitional' justification wears thin while judicial governance expands. Accessibility_collapse (0.55) is intermediate: alternatives (communal autonomy, legislative UCC) are structurally available but politically suppressed. Resistance (0.62) is high from communal boards and traditionalist factions who view judicial harmonization as majoritarian imposition.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, this is genuine coordination: imposing a constitutional floor solves the collective-action problem of fragmented personal laws producing unequal outcomes. From communal boards' seat, it is extraction: their interpretive authority is transferred to a non-representative court. From gender_rights_litigants' seat, it is a fragile scaffold: protections exist only while judicial majorities hold. The engine computes these divergences from the structural data — the claimed scaffold type reflects the authoring seat's structural assessment that the coordination function is real but transitional.
 *
 * DIRECTIONALITY LOGIC:
 *   The supreme_judiciary is the structural beneficiary (d ≈ 0.15): it gains institutional authority, legitimacy capital, and agenda-setting power without legislative risk. Gender_rights_litigants are secondary beneficiaries (d ≈ 0.35): they gain protections but must litigate case-by-case with no guaranteed outcome. Communal_personal_law_boards are primary payers (d ≈ 0.75): they lose monopoly authority and face precedent erosion. Traditionalist_litigants are payers (d ≈ 0.65): they bear compliance costs and symbolic loss. The legislature is excluded (d ≈ 0.5): it avoids political cost of UCC but loses marriage law authority to courts. Directionality is driven by who controls the constitutional floor's content and who bears the cost of its imposition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legislative vacuum on UCC — persists (founding_problem_status: contested). The judiciary's expanded role was explicitly justified as interim; four decades later, the interim has become the operating regime. The scaffold classification captures this tension: if UCC legislation occurs, the constraint dissolves (world_rearranges toward legislative harmonization). If judicial harmonization continues indefinitely without UCC, the constraint becomes a piton (theatrical transition) or tangled_rope (judiciary as entrenched beneficiary). The mandatrophy is unresolved — the mandate has outlived its transitional justification but the sunset mechanism (legislative UCC) remains formally live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct normative reading of the marriage_authority kernel, or an institutional mechanism description that cuts across readings?',
    'Analyze whether the judicial harmonization pathway structurally requires a specific normative commitment (e.g., constitutional supremacy as interpretive lodestar) or merely describes a procedural channel available under multiple normative frameworks.',
    'If mechanism-only, this reading does not have a unique ε referent — it would be a cross-reading institutional variable rather than a kernel reading. This would require decomposing the kernel differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether judicial_harmonization_reading is a normative reading or an institutional mechanism').

omega_variable(
    judiciary_as_beneficiary_extraction,
    'Does the Supreme Court''s acquisition of de facto marriage authority constitute extractive rent-seeking or genuine coordination function?',
    'Compare the judiciary''s resource expenditure (case management, opinion drafting, enforcement monitoring) against the institutional capital and legitimacy gains. Track whether the court''s expanded role correlates with budgetary capture, patronage networks, or merely symbolic authority.',
    'If extractive, the scaffold characterization shifts toward tangled_rope or snare with judiciary as concentrated beneficiary. If coordinative, the scaffold reading holds with judiciary as transitional coordinator.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judiciary_as_beneficiary_extraction, empirical, 'Whether judicial expansion of marriage authority is extractive or coordinative').

omega_variable(
    scaffold_sunset_credibility,
    'Is the implicit sunset (UCC legislation) a genuine transitional target or a perpetual horizon that legitimizes indefinite judicial governance?',
    'Track legislative action on UCC over time. If UCC bills are introduced but never advance, or if judicial harmonization expands in scope whenever legislative momentum stalls, the sunset is performative.',
    'If sunset is performative, reclassify from scaffold to piton (theatrical transition) or tangled_rope (extraction masked as transition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_sunset_credibility, empirical, 'Whether the UCC sunset clause is genuine or performative').

omega_variable(
    cs_framing_underdetermination,
    'Does the commitment-system structure describe the judiciary''s authority over marriage, or the constitutional text''s authority that the judiciary claims to interpret?',
    'Compare cs_structure outputs under two framings: (a) kernel = constitutional equality provisions, authority_grounding = lineage/extraction (judiciary as interpreter); (b) kernel = marriage authority itself, authority_grounding = practice (judiciary as de facto author). Assess which produces coherent axioms and drift_state.',
    'Different framings yield different cs_pattern classifications and different reading_relations to siblings. The choice of kernel object is not neutral.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the kernel is constitutional text or marriage authority itself').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__judicial_harmonization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mahr_tr_t0, marriage_authority__judicial_harmonization_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(mahr_tr_t0, observed).
narrative_ontology:measurement(mahr_tr_t10, marriage_authority__judicial_harmonization_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(mahr_tr_t10, observed).
narrative_ontology:measurement(mahr_tr_t20, marriage_authority__judicial_harmonization_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(mahr_tr_t20, observed).
narrative_ontology:measurement(mahr_tr_t30, marriage_authority__judicial_harmonization_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(mahr_tr_t30, observed).
narrative_ontology:measurement(mahr_tr_t40, marriage_authority__judicial_harmonization_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(mahr_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(mahr_be_t0, marriage_authority__judicial_harmonization_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(mahr_be_t0, observed).
narrative_ontology:measurement(mahr_be_t10, marriage_authority__judicial_harmonization_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement_basis(mahr_be_t10, observed).
narrative_ontology:measurement(mahr_be_t20, marriage_authority__judicial_harmonization_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement_basis(mahr_be_t20, observed).
narrative_ontology:measurement(mahr_be_t30, marriage_authority__judicial_harmonization_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(mahr_be_t30, observed).
narrative_ontology:measurement(mahr_be_t40, marriage_authority__judicial_harmonization_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(mahr_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(mahr_su_t0, marriage_authority__judicial_harmonization_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(mahr_su_t0, observed).
narrative_ontology:measurement(mahr_su_t10, marriage_authority__judicial_harmonization_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement_basis(mahr_su_t10, observed).
narrative_ontology:measurement(mahr_su_t20, marriage_authority__judicial_harmonization_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement_basis(mahr_su_t20, observed).
narrative_ontology:measurement(mahr_su_t30, marriage_authority__judicial_harmonization_reading, suppression_requirement, 30, 0.32).
narrative_ontology:measurement_basis(mahr_su_t30, observed).
narrative_ontology:measurement(mahr_su_t40, marriage_authority__judicial_harmonization_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement_basis(mahr_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__judicial_harmonization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority__judicial_harmonization_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__federalist_millet_reading).

% DUAL FORMULATION NOTE:
% The marriage_authority kernel decomposes into five constraint stories linked by affects_constraints. This reading (judicial_harmonization) is the institutional mechanism pathway; gender_rights_reading is the normative driver; secularist_reading is the legislative endpoint; communal_autonomy_reading and federalist_millet_reading are the resistance baselines. The judicial harmonization pathway structurally influences gender_rights_reading (provides enforcement channel) and secularist_reading (delays legislative UCC by satisfying partial demand), while being resisted by communal_autonomy_reading and federalist_millet_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__judicial_harmonization_reading, institutional, 0.15).
constraint_indexing:directionality_override(marriage_authority__judicial_harmonization_reading, organized, 0.75).
constraint_indexing:directionality_override(marriage_authority__judicial_harmonization_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
