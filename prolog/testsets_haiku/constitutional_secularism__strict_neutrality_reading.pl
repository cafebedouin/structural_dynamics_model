% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__strict_neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__strict_neutrality_reading, []).

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
 *   constraint_id: constitutional_secularism__strict_neutrality_reading
 *   human_readable: Constitutional Secularism: Strict Neutrality Reading
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   Constitutional secularism — the principle that the state maintains equal
 *   distance from all religions — is a contested kernel instantiated through
 *   multiple readings. The STRICT NEUTRALITY READING claims the state must
 *   maintain complete non-interference: no preferential treatment, no
 *   intervention in internal religious affairs, no mobilization of state
 *   machinery for either majoritarian establishment or minority protection
 *   from internal oppression. This reading frames neutrality as the cure for
 *   sectarian conflict and a guardrail against state capture by religious
 *   actors. However, the structural impact is that state capacity to protect
 *   vulnerable sub-groups within traditions (women, LGBTQ members, apostates,
 *   internal reformers) is unavailable, and community internal hierarchies
 *   are ratified as autonomous. The strict neutrality reading competes with a
 *   PRINCIPLED INTERVENTION READING (state may ally with internal reformers
 *   and protect intra-community vulnerability) and a REFORMIST READING (state
 *   has affirmative duty to eliminate oppressive practices, superseding
 *   community autonomy). This JSON instantiates the strict neutrality reading
 *   only; the sibling readings are separate constraint stories in the family.
 *
 * KEY AGENTS:
 *   - Secular governance institutions (state courts, legislatures, enforcement agencies) — agenda setter; administers the boundary between religious and secular spheres
 *   - Religious minorities — beneficiary (protected from majoritarian state action) but also payer (lose state alliance for internal reform)
 *   - Majority religious communities — payer; lose preferential state treatment and control over state machinery
 *   - Internal reformers within traditions — payer/excluded; trapped between community loyalty and state indifference; identity-locked
 *   - State capacity advocates and reformist bureaucrats — excluded; barred from intervention agenda
 *   - External observers (human rights bodies, diaspora) — analytical; monitor outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.62).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.41).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Constitutional Secularism: Strict Neutrality Reading").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, '7b8752a0-ba7e-4b5d-abf0-84c86fe3f41e').
narrative_ontology:cs_kernel_codification('7b8752a0-ba7e-4b5d-abf0-84c86fe3f41e', formalized).
narrative_ontology:cs_authority_grounding('7b8752a0-ba7e-4b5d-abf0-84c86fe3f41e', lineage).
narrative_ontology:cs_interpretation_layer_present('7b8752a0-ba7e-4b5d-abf0-84c86fe3f41e').
narrative_ontology:cs_reading_relation('7b8752a0-ba7e-4b5d-abf0-84c86fe3f41e', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b8752a0-ba7e-4b5d-abf0-84c86fe3f41e', constitutional_secularism__reformist_reading, influences).
narrative_ontology:cs_axiom('7b8752a0-ba7e-4b5d-abf0-84c86fe3f41e', foundational, state_neutrality_prerequisite_for_religious_freedom).
narrative_ontology:cs_axiom_status(state_neutrality_prerequisite_for_religious_freedom, holdable).
narrative_ontology:cs_axiom_grounding('7b8752a0-ba7e-4b5d-abf0-84c86fe3f41e', state_neutrality_prerequisite_for_religious_freedom, deontological).
narrative_ontology:cs_axiom('7b8752a0-ba7e-4b5d-abf0-84c86fe3f41e', foundational, community_autonomy_supersedes_internal_vulnerability).
narrative_ontology:cs_axiom_status(community_autonomy_supersedes_internal_vulnerability, holdable).
narrative_ontology:cs_axiom_grounding('7b8752a0-ba7e-4b5d-abf0-84c86fe3f41e', community_autonomy_supersedes_internal_vulnerability, deontological).
narrative_ontology:cs_reference_frame('7b8752a0-ba7e-4b5d-abf0-84c86fe3f41e', secular_separation_doctrine).
narrative_ontology:cs_drift_state('7b8752a0-ba7e-4b5d-abf0-84c86fe3f41e', contemporary_pluralist_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7b8752a0-ba7e-4b5d-abf0-84c86fe3f41e', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, religious_minorities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, secular_governance_institutions).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, majority_religious_communities).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, internal_reformers_within_traditions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State courts, legislatures, and executive enforcement agencies maintain the secularist reading by refusing to adjudicate internal religious disputes, rejecting pleas for state intervention against practices deemed oppressive, and treating all religious traditions identically under law regardless of their internal hierarchies or reform demands. They justify this as protecting religious autonomy and preventing majoritarian capture; they administer the boundary between religious and secular spheres.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, secular_governance_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Gain protection from majoritarian state action — the state will not legislate against minority religious practices or attempt to 'reform' their traditions on majoritarian grounds. They retain internal institutional autonomy. But they also experience the constraint's refusal to intervene when majorities within their own communities (elders, male heads) oppress minorities within minorities (women, LGBTQ members, apostates, dissidents).
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_minorities, beneficiary,
    moderate, generational, mobile, national).

% Bear the cost of state non-intervention in their internal affairs and equal legal treatment despite numerical dominance. They lose the ability to use state machinery to enforce their tradition's doctrines, convert populations, or establish preferential institutional arrangements. They argue the neutrality principle forces them to subsidize minority practices without receiving reciprocal state support for their own.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, majority_religious_communities, payer,
    powerful, generational, constrained, national).

% Activists and scholars within religious traditions seeking to reform oppressive internal practices (gender inequality, caste systems, conversion punishments, LGBTQ exclusion) lose the option of state alliance. The strict neutrality rule treats their internal reformist agenda as a majoritarian imposition rather than a liberation movement. They are caught between community loyalty (which enforces against dissent) and state indifference (which refuses to side with them against their own tradition's hierarchy).
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, internal_reformers_within_traditions, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__strict_neutrality_reading, internal_reformers_within_traditions, excluded).

% State officials, legislators, and reformist bureaucrats who believe the state has both capacity and mandate to intervene in religious communities to protect vulnerable sub-groups are structurally barred from acting. Their reform agenda is treated as preferential treatment and sectarian overreach under the strict neutrality reading. They are kept out of the conversation about when community autonomy should yield to protection of the vulnerable.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, state_capacity_advocates, excluded,
    institutional, biographical, analytical, national).

% International human rights bodies and diaspora communities monitor whether the neutrality principle actually protects or abandons vulnerable sub-groups. They produce testimony about outcomes: whether strict neutrality shields minorities from majoritarian law or shields majorities from reform pressure, and at whose expense.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_minorities_externally, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The state abstracts away from sectarian preference and theological disputes by treating all religious traditions identically under secular law: no tradition receives state validation, resources, or enforcement of doctrinal claims; conversely, none faces state opposition on theological grounds. This solves the founding problem of sectarian conflict by making state power unavailable as a prize in religious competition.
% TRANSFER_FUNCTION: The constraint transfers state capacity for social reform away from internal-minority liberation movements (particularly women, apostates, LGBTQ members, dissidents within traditions) and away from majoritarian communities' claims for preferential treatment, lodging both in community-internal self-governance. Majorities within traditions retain control over minorities within their own communities; state neutrality ratifies internal hierarchies as autonomous choices.
% ABSENT_VOICES: Internal reformers and sub-group minorities within religious communities are excluded — not formally, but structurally, because state neutrality treats their reform agenda as preferential intervention rather than liberation. State capacity advocates and those who believe vulnerability within communities outweighs autonomy-of-tradition are also kept out of the authoritative conversation, though they testify in legislatures and courts.
% DISAPPEARANCE_RATIONALE: If strict neutrality vanished and the state became available as an ally for both reformist movements within traditions and majoritarian communities seeking preferential treatment, the entire institutional ecology would restructure: state machinery would become a contested prize in religious competition, alliances would shift from within-community to state-community, and the constitutional settlement separating religious from secular law would dissolve.
% FOUNDING_PROBLEM: Late-18th and 19th-century European and colonial contexts: sectarian warfare, majoritarian establishment churches using state power to suppress or convert minorities, religious minorities seeking refuge in non-denominational secular law and state neutrality.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship attests the founding problem (sectarian conflict, majoritarian oppression) was real and urgent in the contexts where constitutionalism emerged. Contemporary debate: majoritarian communities argue the founding problem is overcome and neutrality now protects minorities against reform; minority-protection advocates argue the founding problem persists in different form (intra-community oppression now shielded from state intervention) and strict neutrality enables it. No neutral outside corroboration; the disagreement is precisely over what counts as the problem the reading solves.
narrative_ontology:disappearance_verdict(constitutional_secularism__strict_neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__strict_neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__strict_neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__strict_neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__strict_neutrality_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__strict_neutrality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__strict_neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is CLAIMED as tangled_rope (genuine coordination function + asymmetric extraction) and AUTHORED with metrics consistent with substantial extraction. Base extractiveness is moderate-high (0.62 at interval end) because the state's refusal to intervene protects majority internal hierarchies against reform pressure — that protection IS a form of state action that asymmetrically benefits majorities and harms reformers. Suppression is moderate (0.41) because the constraint is not enforced through overt coercion but through institutional design (courts refusing jurisdiction, legislatures forbidding sectarian lawmaking). Theater is rising modestly (0.12 → 0.28) because enforcing neutrality in practice requires performative demonstrations of equal distance even when material treatment diverges (courts stating they protect religious freedom while declining to intervene in oppressive practices). The measurement series shows extraction and theater both rising slowly over 40 time units as the constraint's operation matures and the gap between stated neutrality and structural preference for internal-majority control becomes more apparent. Suppression_requirement rises as more reformist pressure emerges and must be managed through institutional resistance. One shared time grid: all metrics are authored at all 6 time points.
 *
 * PERSPECTIVAL GAP:
 *   Secular institutions compute the constraint as low-extraction rope (coordination that benefits all parties equally by removing religion from state machinery). Internal reformers and vulnerable sub-group seats compute it as snare (extraction protecting majority hierarchies against reform). Religious minorities compute it as asymmetric rope (coordination that protects them from majorities but exposes them to internal oppression). The divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular governance institutions (institutional power, analytical exit) are the beneficiaries in the classification sense — they get the benefit of the boundary rule itself, which shields them from demands to adjudicate religious disputes and keeps state machinery formally uncontaminated by sectarian claims. Religious minorities (moderate power, mobile exit) are nominal beneficiaries (protected from majoritarian state action) but also partial payers (lose alliance option for internal reform). Majority religious communities (powerful, constrained exit) are clear payers — they lose state machinery. Internal reformers (moderate power, identity_locked exit) are the deepest targets — they are trapped between expelled from state (which will not ally with them) and expelled from community (which punishes dissent). The constraint's directionality is d ≈ 0.7 for internal reformers (high targets), d ≈ 0.3 for religious minorities (partial beneficiaries), d ≈ 0.6 for majorities (clear payers), d ≈ 0.15 for secular institutions (beneficiaries of the boundary). No directionality overrides are needed; the structural derivation from exit + power is accurate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sectarian warfare, majoritarian oppression) was live when the constraint was established. In contemporary pluralist democracies with established rule-of-law institutions, the founding problem is substantially contained — sectarian violence is low, majoritarian religious establishments are weakened. However, a NEW problem emerged: intra-community oppression of sub-groups (women, LGBTQ members, apostates) formerly protected only by state intervention is now shielded from state intervention by the neutrality principle. Some observers argue the founding problem is DEAD (the constraint now protects hierarchies more than it liberates minorities from majoritarian capture). Others argue it is CONTESTED (it depends on whether you weigh inter-community equality against intra-community equality). The constraint shows signs of mandatrophy: it persists because no party has built the political power to overturn it in favor of one of the rival readings, NOT because the founding problem remains pressing. Theater rising to 0.28 (modest but steady) is consistent with mandatrophy's signature of performative maintenance — neutral-distance rhetoric masking asymmetric structural effect. The constraint is not a piton yet (institutions still defend it; theater is still moderate), but the trajectory toward mandatrophy is visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neutrality_vs_protection_irreconcilability,
    'Is absolute state neutrality structurally compatible with protecting vulnerable sub-groups within religious communities from internal oppression?',
    'Counterfactual policy analysis: design a state intervention regime that protects intra-community vulnerability without enabling majoritarian intervention, or prove that any such intervention logically invokes sectarian judgment.',
    'If incompatible, the strict neutrality reading logically FORECLOSES the principled intervention and reformist readings. If compatible, the readings COEXIST as live alternatives reflecting different priorities (autonomy vs. protection). The resolution determines whether readings are incommensurable or merely rival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_vs_protection_irreconcilability, conceptual, 'Whether strict neutrality and intra-community protection are logically compatible or mutually exclusive.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression (0.41) structural (external barriers to reform access state machinery) or internalized (reformers accept the neutrality principle and do not demand state intervention)?',
    'Post-constraint-revision trajectories: if suppression persists after state legal barriers to intervention are removed, reclassify as partially internalized (reformers have absorbed the principle as legitimate).',
    'If primarily internalized, effective suppression is higher than the structural measure suggests — the constraint has colonized reformers'' own self-concepts. If primarily structural, the suppression is external and would drop if barriers were removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression of reform access is structural or internalized in reformers'' acceptance of neutrality.').

omega_variable(
    founding_problem_persists_or_resolved,
    'Does the founding problem (sectarian conflict, majoritarian oppression) still require strict neutrality to solve, or has it been superseded by new problems (intra-community oppression) that neutrality exacerbates?',
    'Comparative institutional analysis: measure sectarian conflict rates and intra-community protection outcomes in jurisdictions with strict neutrality vs. principled intervention approaches.',
    'If founding problem persists: strict neutrality is still justified, and measured extraction (0.62) is acceptable coordination cost. If new problem dominates: strict neutrality is mandatrophic, and the constraint should transition to one of the rival readings to protect intra-community vulnerability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_persists_or_resolved, empirical, 'Whether the founding problem or its successor problem is the operative constraint on state action.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a reading OF THE SAME KERNEL as the principled intervention and reformist readings, or a fundamentally different constraint sharing a label?',
    'Textual and genealogical analysis: do all three readings cite the same constitutional text or principle (the kernel) and offer competing interpretations, or do they cite different texts/principles entirely? If the latter, decompose into separate kernels.',
    'If same kernel, the three readings are live alternatives and the network relationship is within-family (reading relations + axiom differences). If different kernels, they are separate constraints that happen to compete in the same domain, and network relationships should be affects_constraints rather than reading-family links.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the strict neutrality reading is a reading of a contested kernel or a distinct constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__strict_neutrality_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t8, constitutional_secularism__strict_neutrality_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(cons_tr_t16, constitutional_secularism__strict_neutrality_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(cons_tr_t24, constitutional_secularism__strict_neutrality_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(cons_tr_t32, constitutional_secularism__strict_neutrality_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__strict_neutrality_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cons_be_t8, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(cons_be_t16, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(cons_be_t24, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(cons_be_t32, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cons_su_t8, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 8, 0.31).
narrative_ontology:measurement(cons_su_t16, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(cons_su_t24, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 24, 0.39).
narrative_ontology:measurement(cons_su_t32, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 32, 0.41).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 40, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__strict_neutrality_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(constitutional_secularism__strict_neutrality_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__reformist_reading).

% DUAL FORMULATION NOTE:
% The constitutional secularism kernel (state relationship to religion) is instantiated through three structurally distinct readings: strict neutrality, principled intervention, and reformist. Each reading has different ε, different beneficiary/victim structures, and different classifications. They are linked as members of a constraint family, not as alternative observables of one constraint. The ε-invariance principle requires decomposition: the same text (constitution) instantiates different constraints depending on the reading's core premise about what state action is appropriate. Network edges point downstream from this strict neutrality reading to the two rival readings, because this reading's withdrawal of state capacity creates the vacuum the rival readings propose to fill.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
