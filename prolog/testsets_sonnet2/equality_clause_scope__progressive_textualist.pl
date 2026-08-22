% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__progressive_textualist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__progressive_textualist, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: equality_clause_scope__progressive_textualist
 *   human_readable: Equality Clause Scope — Progressive Textualist Reading (Amendment-Bound Expansion)
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This story authors the progressive textualist reading of the equality
 *   clause scope kernel: the constitutional text contains a genuine equality
 *   principle, but its application scope legitimately expands only through
 *   the democratic supermajority amendment process, not through judicial
 *   reinterpretation of existing text. This is bounded universalism — the
 *   principle's ultimate reach is treated as universal in aspiration, but its
 *   actual legal coverage at any given time is fixed by what has been
 *   formally ratified, not by what a court believes the principle's logic
 *   requires. This reading is distinct from the expansive_universalist
 *   reading (which treats the principle as already covering all humans
 *   regardless of ratification history) and the restrictive_originalist
 *   reading (which caps the principle's proper scope at 18th-century
 *   propertied white male political actors). All three are separate
 *   constraint stories sharing one contested kernel; this file's epsilon is
 *   authored only for the amendment-gated arrangement as this reading itself
 *   understands it.
 *
 * KEY AGENTS:
 *   - legislative_coalition_builders: institutional agenda-setters who control which claims are proposed and ratified
 *   - constitutional_amendment_beneficiary_classes: groups whose inclusion was secured via ratified amendments
 *   - unratified_claimant_classes: powerless, trapped groups awaiting supermajority recognition
 *   - textualist_judiciary: institutional beneficiaries of a low-discretion adjudicative posture
 *   - expansive_universalist_advocates: excluded voices who reject the amendment-gate as the only legitimate expansion path
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.42).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.38).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.42).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__progressive_textualist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__progressive_textualist, "Equality Clause Scope — Progressive Textualist Reading (Amendment-Bound Expansion)").
narrative_ontology:topic_domain(equality_clause_scope__progressive_textualist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__progressive_textualist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__progressive_textualist, 'd7ca462a-fdbc-4426-82ea-791086aaee1a').
narrative_ontology:cs_kernel_codification('d7ca462a-fdbc-4426-82ea-791086aaee1a', fixed_text).
narrative_ontology:cs_authority_grounding('d7ca462a-fdbc-4426-82ea-791086aaee1a', practice).
narrative_ontology:cs_interpretation_layer_present('d7ca462a-fdbc-4426-82ea-791086aaee1a').
narrative_ontology:cs_reading_relation('d7ca462a-fdbc-4426-82ea-791086aaee1a', equality_clause_scope__restrictive_originalist, coexists_with).
narrative_ontology:cs_reading_relation('d7ca462a-fdbc-4426-82ea-791086aaee1a', equality_clause_scope__expansive_universalist, influences).
narrative_ontology:cs_axiom('d7ca462a-fdbc-4426-82ea-791086aaee1a', foundational, scope_expansion_requires_ratified_consent).
narrative_ontology:cs_axiom_status(scope_expansion_requires_ratified_consent, holdable).
narrative_ontology:cs_axiom_grounding('d7ca462a-fdbc-4426-82ea-791086aaee1a', scope_expansion_requires_ratified_consent, conventional).
narrative_ontology:cs_axiom('d7ca462a-fdbc-4426-82ea-791086aaee1a', foundational, judicial_scope_declaration_illegitimate).
narrative_ontology:cs_axiom_status(judicial_scope_declaration_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('d7ca462a-fdbc-4426-82ea-791086aaee1a', judicial_scope_declaration_illegitimate, conventional).
narrative_ontology:cs_axiom('d7ca462a-fdbc-4426-82ea-791086aaee1a', secondary, principle_is_bounded_not_frozen).
narrative_ontology:cs_axiom_status(principle_is_bounded_not_frozen, holdable).
narrative_ontology:cs_axiom_grounding('d7ca462a-fdbc-4426-82ea-791086aaee1a', principle_is_bounded_not_frozen, instrumental).
narrative_ontology:cs_reference_frame('d7ca462a-fdbc-4426-82ea-791086aaee1a', textual_principle_with_procedural_expansion).
narrative_ontology:cs_drift_state('d7ca462a-fdbc-4426-82ea-791086aaee1a', post_era_ratification_failure_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d7ca462a-fdbc-4426-82ea-791086aaee1a', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__progressive_textualist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, constitutional_amendment_beneficiary_classes).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, legislative_coalition_builders).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, textualist_judiciary).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, unratified_claimant_classes).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, supermajority_blocked_minorities).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, democratic_legitimacy_of_constitutional_change).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, textual_fidelity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups whose equal-protection status was secured through ratified amendments (e.g., formerly enslaved persons via the Thirteenth/Fourteenth Amendments, women via the Nineteenth). Under this reading, their inclusion is legitimate precisely because it passed through supermajority consent, giving their status durable textual anchoring rather than a court-dependent holding that could be reargued.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, constitutional_amendment_beneficiary_classes, beneficiary,
    organized, generational, constrained, national).

% Legislators, state ratifying conventions, and political movements that assemble the supermajorities required to amend the text. They control the pace and scope of equality's expansion, deciding which claims get proposed for ratification and which are left outside the constitutional floor. Their gatekeeping role is the mechanism the reading depends on.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, legislative_coalition_builders, agenda_setter,
    institutional, generational, arbitrage, national).

% Courts that apply this reading gain a stable, low-discretion adjudicative posture: they enforce whatever scope the amended text actually contains, deflecting responsibility for controversial expansions onto the democratic process. This preserves judicial legitimacy and insulates judges from charges of policymaking, at the cost of leaving unratified claims outside the clause's reach.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, textualist_judiciary, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__progressive_textualist, textualist_judiciary, agenda_setter).

% Groups whose equality claims have not yet cleared the amendment threshold — historically, this included women before 1920, and includes today's claimants (e.g., LGBTQ+ persons, certain disability classes) who argue the equality principle's logic already covers them. Under this reading, they must wait for supermajority ratification; judicial recognition of their claim as already covered by existing text is foreclosed as illegitimate.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, unratified_claimant_classes, payer,
    powerless, biographical, trapped, national).

% Numerically small or geographically concentrated minorities whose equality claims cannot realistically clear a two-thirds congressional and three-fourths state ratification bar, regardless of the claim's moral merit. The amendment threshold structurally disadvantages any group too small or too regionally isolated to build a nationwide supermajority coalition.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, supermajority_blocked_minorities, payer,
    powerless, generational, trapped, national).

% Advocates who believe the equality principle already covers excluded groups as a matter of the text's inherent logic, and that courts should recognize this without waiting for amendment. This reading treats their claim as a category error — conflating moral truth with constitutional scope — and excludes their preferred remedy (judicial declaration) from the legitimate toolkit.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, expansive_universalist_advocates, excluded,
    organized, generational, constrained, national).

% Scholars who trace how the amendment process has actually expanded equality's scope over time, documenting both the legitimacy the process confers and the delay and exclusion it produces for groups unable to muster supermajorities.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__progressive_textualist, diffuse).
narrative_ontology:fixing_cost_class(equality_clause_scope__progressive_textualist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, broadly legitimate mechanism for expanding who counts within the equality principle: because expansion requires supermajority consent across diverse coalitions, once achieved it is durable and widely accepted rather than contingent on the composition of a court.
% TRANSFER_FUNCTION: Moves the power to define equality's scope from judicial interpretation to legislative and popular coalition-building; this shifts legitimacy and durability toward ratified beneficiary classes and shifts delay, uncertainty, and continued exclusion onto claimant classes who cannot yet assemble a supermajority.
% ABSENT_VOICES: Expansive universalist advocates who believe the text's equality logic already covers excluded groups are structurally excluded from the legitimate remedy path; their argument that courts should recognize inherent scope is treated as illegitimate under this reading, not merely unpersuasive.
% DISAPPEARANCE_RATIONALE: If the requirement that equality's scope expand only through amendment vanished, courts would gain authority to recognize excluded groups' claims directly, dramatically accelerating recognition for currently unratified classes and reshaping the balance of power between judiciary and legislature on civil rights questions.
% FOUNDING_PROBLEM: The founding problem this reading answers is the legitimacy question: how can a constitutional equality principle expand its coverage over time without collapsing into either judicial fiat (a court simply declaring new groups included) or permanent textual ossification (a principle frozen in its 18th-century application)?
% FOUNDING_PROBLEM_CORROBORATION: Legislative historians and framers of Reconstruction-era amendments attest that supermajority ratification was deliberately chosen to secure durable legitimacy for expanded equality claims. However, civil rights litigators and constitutional historians outside the amendment-process's beneficiary coalitions attest that the threshold has also been used to indefinitely stall claims (e.g., the stalled Equal Rights Amendment) that command broad public support but not the specific supermajority geography required — corroboration for the founding problem's continued live status is mixed and comes from parties on both sides of the amendment gate.
narrative_ontology:disappearance_verdict(equality_clause_scope__progressive_textualist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__progressive_textualist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__progressive_textualist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equality_clause_scope__progressive_textualist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__progressive_textualist, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__progressive_textualist_tests).
:- end_tests(equality_clause_scope__progressive_textualist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42): the arrangement genuinely coordinates legitimate constitutional change but imposes real, sometimes decades-long costs on claimant classes who cannot assemble supermajorities, particularly where support is broad but geographically concentrated (the stalled ERA is the clearest illustration). Suppression is moderate (0.38) — it is not a coercive suppression of dissent so much as a structural raising of the bar that keeps some claims permanently short of legal recognition despite popular sentiment. Theater rose around 1982 reflecting increased performative invocation of 'the amendment process is open to you' rhetoric during a period (post-ERA failure) when the practical path had become harder to traverse; it eased somewhat since as originalist and living-constitutionalist debates absorbed more of the discourse. Accessibility collapse (0.45) is moderate: the amendment path remains formally open (unlike a mountain) but is practically foreclosed for many claims given modern polarization, hence real resistance (0.55) from advocacy groups pushing for judicial recognition instead.
 *
 * DIRECTIONALITY LOGIC:
 *   Legislative coalition-builders and the textualist judiciary sit near the beneficiary end: they retain or gain institutional authority and legitimacy by channeling equality expansion through the amendment gate rather than ceding interpretive authority to courts. Ratified beneficiary classes also sit toward the beneficiary end — their status is textually secure once ratified. Unratified claimant classes and supermajority-blocked minorities sit near the full-target end: they are powerless, trapped (their claims cannot be litigated into recognition under this reading), and bear the ongoing cost of exclusion pending a political process they often cannot move. This is a genuine tangled rope: coordination (durable, broadly legitimate constitutional change) and extraction (indefinite postponement imposed on groups unable to clear a demanding threshold) run through the same structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legitimating equality's expansion without collapsing into either judicial fiat or textual ossification — is genuinely contested rather than settled. It remains partially live (new claimant groups continue to seek recognition) but the mechanism itself has also become a tool for indefinite postponement independent of its original legitimating purpose, particularly where geographic supermajority requirements no longer track actual national consensus (mismatched Senate/state ratification math vs. popular vote distributions). This divergence between the mechanism's stated purpose and its de facto blocking function is exactly the kind of drift the tangled_rope classification is designed to surface rather than let default to either pure rope (all coordination) or pure snare (all extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_gate_legitimate_or_obstructive,
    'Is the supermajority amendment requirement a genuine legitimacy-conferring coordination mechanism, or has it become primarily an obstructive veto point exploited by geographically concentrated minorities to block popular equality claims?',
    'Compare historical amendment success rates and time-to-ratification against measured national public opinion on equality claims across decades; a persistent gap between majority support and ratification success indicates obstruction rather than legitimate deliberation.',
    'If the gate has become primarily obstructive, the tangled_rope classification tips toward snare for currently-blocked claimant classes even though the historical function was genuinely coordinative; if the gate still tracks genuine deliberative consensus-building, the tangled_rope framing (real coordination plus real but bounded cost) remains apt.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_gate_legitimate_or_obstructive, empirical, 'Whether the amendment threshold still functions as legitimacy-building or has become primarily obstructive.').

omega_variable(
    kernel_framing_committer_structure,
    'Which reading of the equality clause''s scope-expansion mechanism is the correct structural account: text-plus-amendment (this reading), text-plus-inherent-logic (expansive_universalist), or text-plus-original-limitation (restrictive_originalist)?',
    'This is a contested normative/interpretive question without a data-resolvable answer; the framework treats each reading as a separate constraint rather than adjudicating between them, per the ε-invariance and kernel-reading principles.',
    'The choice of reading determines which agents count as legitimate beneficiaries versus illegitimately excluded claimants, and determines whether the same historical facts (e.g., women''s suffrage requiring a formal amendment) are read as evidence of a working, legitimate system or evidence of an unjustly slow one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_committer_structure, conceptual, 'The kernel-level interpretive disagreement this reading is one instantiation of.').

omega_variable(
    geographic_supermajority_distortion,
    'Does the state-based ratification math (three-fourths of states, regardless of population) systematically distort which equality claims can pass, favoring claims popular in smaller/rural states over claims popular nationally but concentrated in fewer, larger states?',
    'Statistical analysis comparing population-weighted national support for various equality claims (historical and contemporary) against the state-count-weighted ratification math required to amend.',
    'If a systematic distortion exists, it would show the amendment gate does not track ''democratic consent'' evenly across claim types, undermining the reading''s core legitimacy claim for certain classes of claimants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_supermajority_distortion, empirical, 'Whether the ratification formula itself introduces a structural bias independent of the amendment process''s stated purpose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 1868, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1868, equality_clause_scope__progressive_textualist, theater_ratio, 1868, 0.1).
narrative_ontology:measurement(equa_tr_t1900, equality_clause_scope__progressive_textualist, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(equa_tr_t1920, equality_clause_scope__progressive_textualist, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(equa_tr_t1965, equality_clause_scope__progressive_textualist, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(equa_tr_t1982, equality_clause_scope__progressive_textualist, theater_ratio, 1982, 0.28).
narrative_ontology:measurement(equa_tr_t2026, equality_clause_scope__progressive_textualist, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(equa_be_t1868, equality_clause_scope__progressive_textualist, base_extractiveness, 1868, 0.5).
narrative_ontology:measurement(equa_be_t1900, equality_clause_scope__progressive_textualist, base_extractiveness, 1900, 0.48).
narrative_ontology:measurement(equa_be_t1920, equality_clause_scope__progressive_textualist, base_extractiveness, 1920, 0.4).
narrative_ontology:measurement(equa_be_t1965, equality_clause_scope__progressive_textualist, base_extractiveness, 1965, 0.38).
narrative_ontology:measurement(equa_be_t1982, equality_clause_scope__progressive_textualist, base_extractiveness, 1982, 0.46).
narrative_ontology:measurement(equa_be_t2026, equality_clause_scope__progressive_textualist, base_extractiveness, 2026, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1868, equality_clause_scope__progressive_textualist, suppression_requirement, 1868, 0.55).
narrative_ontology:measurement(equa_su_t1900, equality_clause_scope__progressive_textualist, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(equa_su_t1920, equality_clause_scope__progressive_textualist, suppression_requirement, 1920, 0.42).
narrative_ontology:measurement(equa_su_t1965, equality_clause_scope__progressive_textualist, suppression_requirement, 1965, 0.35).
narrative_ontology:measurement(equa_su_t1982, equality_clause_scope__progressive_textualist, suppression_requirement, 1982, 0.4).
narrative_ontology:measurement(equa_su_t2026, equality_clause_scope__progressive_textualist, suppression_requirement, 2026, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__expansive_universalist).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__restrictive_originalist).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language concept 'the equality clause's proper scope' per the ε-invariance principle. Each reading (progressive_textualist here, plus expansive_universalist and restrictive_originalist as separate files) authors its own ε against its own understanding of the standing arrangement, because the three readings dispute not just whether the arrangement is good but what the arrangement structurally IS. All three link to each other via affects_constraints to preserve the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
