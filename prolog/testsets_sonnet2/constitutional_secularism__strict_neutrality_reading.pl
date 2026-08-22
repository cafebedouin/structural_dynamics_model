% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__strict_neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   constraint_id: constitutional_secularism__strict_neutrality_reading
 *   human_readable: Constitutional Secularism — Strict Equidistance Reading
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   A constitutional order commits the state to treating all religious
 *   communities equally, neither favoring one over another nor intervening in
 *   their internal religious affairs. Under the strict neutrality reading,
 *   this is operationalized as near-uniform non-interference: courts and
 *   legislatures decline to adjudicate practices framed as internal to a
 *   religious community's faith and practice, even where those practices harm
 *   identifiable members within the community. The doctrine solves a real
 *   coordination problem (preventing the state from becoming a vehicle for
 *   majoritarian religious capture) but the same non-interference rule leaves
 *   internally vulnerable members — women, dissenters, subordinated sects —
 *   without a state remedy, because any remedy would require the state to
 *   take a substantive position on which religious practices are acceptable,
 *   which equidistance forbids.
 *
 * KEY AGENTS:
 *   - state_judiciary_and_executive: agenda_setter (institutional/analytical) — administers and enforces the equidistance rule
 *   - majority_religious_establishment: beneficiary (organized/arbitrage) — benefits from status quo distributions left uncorrected
 *   - intra_community_dissenters: payer (powerless/trapped) — bears cost of non-intervention within their own community
 *   - women_within_minority_religious_communities: payer (powerless/trapped) — subject to personal-law regimes the state will not override
 *   - religious_minority_institutions: beneficiary/agenda_setter (organized/constrained) — retains self-governance shielded from state correction
 *   - constitutional_courts: observer (institutional/analytical) — periodically forced to draw the line the doctrine tries to avoid drawing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__strict_neutrality_reading, 0.42).
domain_priors:suppression_score(constitutional_secularism__strict_neutrality_reading, 0.38).
domain_priors:theater_ratio(constitutional_secularism__strict_neutrality_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_secularism__strict_neutrality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__strict_neutrality_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__strict_neutrality_reading, "Constitutional Secularism — Strict Equidistance Reading").
narrative_ontology:topic_domain(constitutional_secularism__strict_neutrality_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__strict_neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__strict_neutrality_reading, 'c25d7bf2-b8f8-4380-9375-9c2b5f5225db').
narrative_ontology:cs_kernel_codification('c25d7bf2-b8f8-4380-9375-9c2b5f5225db', formalized).
narrative_ontology:cs_authority_grounding('c25d7bf2-b8f8-4380-9375-9c2b5f5225db', lineage).
narrative_ontology:cs_interpretation_layer_present('c25d7bf2-b8f8-4380-9375-9c2b5f5225db').
narrative_ontology:cs_reading_relation('c25d7bf2-b8f8-4380-9375-9c2b5f5225db', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_reading_relation('c25d7bf2-b8f8-4380-9375-9c2b5f5225db', constitutional_secularism__reformist_reading, influences).
narrative_ontology:cs_axiom('c25d7bf2-b8f8-4380-9375-9c2b5f5225db', foundational, state_neutrality_requires_non_interference).
narrative_ontology:cs_axiom_status(state_neutrality_requires_non_interference, holdable).
narrative_ontology:cs_axiom_grounding('c25d7bf2-b8f8-4380-9375-9c2b5f5225db', state_neutrality_requires_non_interference, deontological).
narrative_ontology:cs_axiom('c25d7bf2-b8f8-4380-9375-9c2b5f5225db', secondary, equidistance_forecloses_selective_state_correction).
narrative_ontology:cs_axiom_status(equidistance_forecloses_selective_state_correction, holdable).
narrative_ontology:cs_axiom_grounding('c25d7bf2-b8f8-4380-9375-9c2b5f5225db', equidistance_forecloses_selective_state_correction, conventional).
narrative_ontology:cs_reference_frame('c25d7bf2-b8f8-4380-9375-9c2b5f5225db', post_independence_equidistance_settlement).
narrative_ontology:cs_drift_state('c25d7bf2-b8f8-4380-9375-9c2b5f5225db', contemporary_personal_law_litigation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c25d7bf2-b8f8-4380-9375-9c2b5f5225db', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__strict_neutrality_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, majority_religious_establishment).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, state_judiciary_and_executive).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, women_within_minority_religious_communities).
narrative_ontology:constraint_victim(constitutional_secularism__strict_neutrality_reading, lower_caste_and_marginalized_sect_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_secularism__strict_neutrality_reading, religious_minority_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applies a uniform non-interference rule across all recognized religious communities, declining to adjudicate internal religious practices as long as they are framed as matters of faith rather than civil right. Gains legitimacy and reduced political exposure by treating equidistance as principled restraint rather than a discretionary choice about where to draw the line between 'religious' and 'civil'.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, state_judiciary_and_executive, agenda_setter,
    institutional, generational, analytical, national).

% Already holds informal structural advantages (numbers, cultural defaults treated as 'secular' baseline, institutional access) that a formally neutral state leaves undisturbed. Benefits from a rule that treats existing distributions of religious power as the neutral starting point rather than as something requiring correction.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, majority_religious_establishment, beneficiary,
    organized, civilizational, arbitrage, national).

% Members who reject or are harmed by practices enforced within their own religious community (excommunication, personal-law disadvantage, social boycott) find the state declining to intervene on the ground that doing so would breach equidistance. Their only recourse is community-internal channels controlled by the same authorities imposing the harm; formal state neutrality functions as practical abandonment.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, intra_community_dissenters, payer,
    powerless, biographical, trapped, local).

% Subject to personal-law regimes (marriage, divorce, inheritance, custody) administered by religious authorities that the state declines to override in the name of even-handed non-interference. Exit would mean leaving the religious community and its social and economic support entirely, which most cannot afford.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, women_within_minority_religious_communities, payer,
    powerless, biographical, trapped, local).

% Face exclusion or subordination within religious institutional life (temple entry, priesthood access, ritual status) that predates the constitutional order. Strict neutrality treats these as internal religious matters equally shielded from state correction, so historical hierarchies persist under the same rule that protects majority and minority religious autonomy alike.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, lower_caste_and_marginalized_sect_adherents, payer,
    powerless, generational, constrained, regional).

% Retain self-governance over internal religious and personal-law matters without state-mandated reform, which the leadership values as protection against majoritarian pressure and cultural assimilation. Administers internal community rules with limited external check, benefiting from the same non-interference that burdens dissenters within its own ranks.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, religious_minority_institutions, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__strict_neutrality_reading, religious_minority_institutions, agenda_setter).

% Advocate for state intervention against practices they view as oppressive within religious communities, but strict equidistance doctrine treats such advocacy as itself a threat to neutrality, sidelining their proposals as impermissible favoritism toward one normative vision of reform over religious autonomy.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, reform_minded_legislators_and_civil_society, excluded,
    moderate, biographical, constrained, national).

% Periodically asked to draw the line between protected religious practice and civil-rights violation; strict neutrality doctrine constrains how far courts can go without being accused of abandoning equidistance, so judicial intervention is intermittent, narrow, and contested.
narrative_ontology:constraint_stakeholder(constitutional_secularism__strict_neutrality_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__strict_neutrality_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_secularism__strict_neutrality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the state from becoming an instrument of any single religious community's dominance, reducing the risk of state-sponsored religious conflict and allowing plural communities to coexist under a common civil order without fear of majoritarian capture of state machinery.
% TRANSFER_FUNCTION: Moves the cost of internal religious reform away from the state and onto the most vulnerable members of each religious community — dissenters, women, and subordinated sects — who bear the burden of practices the state declines to correct in the name of even-handed non-interference.
% ABSENT_VOICES: Intra-community reformers, especially women and lower-status adherents seeking state protection against practices sanctioned by their own religious leadership, are structurally absent from the equidistance calculus — their claims are recast as internal religious matters rather than civil rights matters, and community leadership speaks for them in the public conversation about neutrality.
% DISAPPEARANCE_RATIONALE: If strict equidistance were abandoned overnight, the state would have to actively choose when and how to intervene in religious practice, community leaderships would lose their current insulation from civil-rights scrutiny, and political coalitions built around 'the state does not touch our internal affairs' would have to renegotiate their terms — a substantial rearrangement of intra-community and state-community relations.
% FOUNDING_PROBLEM: Communal violence and majoritarian capture of state institutions in the transition to independence created a felt need for a state that would not be seen to favor any religion, to hold together a religiously plural polity without triggering large-scale communal conflict.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative political scientists outside any single religious establishment attest the founding communal-conflict problem was real and substantially addressed by early equidistance practice; independent human-rights bodies and dissenting members of minority communities attest that the same doctrine now also functions to shield internal rights violations from correction, so the founding problem's current status is genuinely disputed rather than settled in either direction.
narrative_ontology:disappearance_verdict(constitutional_secularism__strict_neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__strict_neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__strict_neutrality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__strict_neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__strict_neutrality_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__strict_neutrality_reading_tests).
:- end_tests(constitutional_secularism__strict_neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end) rather than severe because the coordination function — avoiding state-sponsored communal conflict — is genuine and substantial, not merely cover; the cost falls unevenly but is diffuse and slow-accumulating rather than a single concentrated transfer. Suppression is moderate (0.38): the doctrine does not physically coerce dissenters, but it structurally forecloses the avenue (state intervention) that would otherwise be available, which functions as suppression by omission rather than by active force. Theater is rising (0.15 to 0.30) as equidistance increasingly functions as a doctrinal shield invoked to avoid politically costly interventions rather than as a considered balance, even as the underlying coordination rationale persists.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat, equidistance is principled restraint that prevents religious favoritism — a Rope. From an intra-community dissenter's seat, the identical rule is a Tangled Rope at best: the coordination function (avoiding state religious favoritism) is real, but it is inseparably bundled with an extraction the doctrine will not name — the state's refusal to correct internally oppressive practices, which asymmetrically burdens the powerless within each community while leaving communal leaderships' authority over their own members untouched.
 *
 * DIRECTIONALITY LOGIC:
 *   The state and majority establishment sit near the beneficiary end: the state gains legitimacy and reduced political exposure from a bright-line non-interference rule, and the majority establishment's existing advantages are simply left undisturbed by a rule that treats the status quo as the neutral baseline. Intra-community dissenters, minority women, and subordinated-sect adherents sit near the target end: trapped exit options (leaving the religious community costs social and economic support), powerless standing, and no alternative venue for redress feed high effective extraction despite the doctrine's formally even-handed text. Religious minority institutions are dual-positioned: they benefit from protection against majoritarian assimilation pressure (a real coordination good) while also functioning as agenda-setters administering internal rules that burden their own dissenting members.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing majoritarian capture of state institutions and communal violence) remains partly live in a religiously plural polity with a history of communal conflict, which is why this is not classified as a pure snare or piton — there is a genuine, still-operative coordination function. But the doctrine's application has drifted from 'the state should not prefer one religion over another' to 'the state should not touch internal religious hierarchies at all,' which extends the original remedy well past its founding scope and onto a different population (internal dissenters rather than inter-religious majoritarian conflict) than the one it was built to protect. Classifying this as tangled_rope rather than snare or rope captures both halves honestly: the coordination is real, and so is the asymmetric cost borne by identifiable payer groups.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location_of_disagreement,
    'The constitutional_secularism kernel is read three ways (strict_neutrality, principled_intervention, reformist). Where exactly does the disagreement live: is it about the FACTS of what constitutes internal religious harm, or about the NORMATIVE question of whether state neutrality or protection of the vulnerable should take priority when they conflict?',
    'Compare judicial reasoning across cases where the same underlying practice (e.g. a personal-law provision) was litigated under different judicial philosophies; if outcomes diverge primarily on stated normative priority rather than on factual findings about harm, the disagreement is normative, not empirical.',
    'If the disagreement is normative rather than factual, no amount of additional fact-finding will resolve which reading should govern — the choice among readings is a policy/values commitment, not something settled by better evidence about practices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Whether the kernel''s contested readings differ on facts or on values.').

omega_variable(
    sibling_reading_structural_delta,
    'If courts shifted from strict_neutrality_reading to principled_intervention_reading, would minority religious institutions'' beneficiary status flip to victim status, and would today''s intra-community payers become today''s beneficiaries — or would the shift simply relocate the burden onto a different subset (e.g., traditionalist majority-community adherents)?',
    'Comparative study of jurisdictions that have shifted their doctrinal posture (e.g., moved from non-interference to state-mandated reform of personal law) and tracked which populations gained and lost standing.',
    'Determines whether the kernel''s readings represent a genuine sum-positive fix (protecting dissenters without new victims) or a zero-sum reallocation of who bears the cost of unresolved religious-autonomy-versus-individual-rights tension.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, empirical, 'Whether shifting readings relocates the burden rather than removing it.').

omega_variable(
    neutrality_as_natural_baseline,
    'Is the strict neutrality reading''s treatment of the current distribution of religious power as the ''neutral'' starting point itself a substantive normative choice that favors incumbents, or is it a genuinely content-neutral default?',
    'Philosophical and empirical analysis of whether any baseline for state non-interference can be truly content-neutral given unequal starting positions among religious communities, drawing on comparative secularism theory.',
    'If the baseline is not neutral, the doctrine''s claim to even-handedness is itself part of what needs justifying, and its beneficiary/victim structure is less a side effect than a designed feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_as_natural_baseline, conceptual, 'Whether formal neutrality toward an unequal status quo is substantively neutral.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__strict_neutrality_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__strict_neutrality_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t14, constitutional_secularism__strict_neutrality_reading, theater_ratio, 14, 0.18).
narrative_ontology:measurement(cons_tr_t28, constitutional_secularism__strict_neutrality_reading, theater_ratio, 28, 0.21).
narrative_ontology:measurement(cons_tr_t42, constitutional_secularism__strict_neutrality_reading, theater_ratio, 42, 0.25).
narrative_ontology:measurement(cons_tr_t56, constitutional_secularism__strict_neutrality_reading, theater_ratio, 56, 0.28).
narrative_ontology:measurement(cons_tr_t70, constitutional_secularism__strict_neutrality_reading, theater_ratio, 70, 0.3).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cons_be_t14, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 14, 0.32).
narrative_ontology:measurement(cons_be_t28, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 28, 0.36).
narrative_ontology:measurement(cons_be_t42, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 42, 0.39).
narrative_ontology:measurement(cons_be_t56, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 56, 0.4).
narrative_ontology:measurement(cons_be_t70, constitutional_secularism__strict_neutrality_reading, base_extractiveness, 70, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cons_su_t14, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 14, 0.32).
narrative_ontology:measurement(cons_su_t28, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 28, 0.34).
narrative_ontology:measurement(cons_su_t42, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 42, 0.35).
narrative_ontology:measurement(cons_su_t56, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 56, 0.37).
narrative_ontology:measurement(cons_su_t70, constitutional_secularism__strict_neutrality_reading, suppression_requirement, 70, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__strict_neutrality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__strict_neutrality_reading, constitutional_secularism__reformist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the natural-language concept 'constitutional secularism' (kernel_id: constitutional_secularism) per the ε-invariance principle. Each reading authorizes a different state posture toward internal religious practice and produces a different victim set: strict_neutrality_reading (this file) leaves intra-community dissenters unprotected in the name of even-handedness; principled_intervention_reading authorizes selective state correction of oppressive practices, which would instead burden religious institutional authority and traditionalist adherents; reformist_reading asserts an affirmative state duty to eliminate oppressive practices, subordinating religious autonomy claims entirely, which would produce the widest victim set among religious institutions and orthodox adherents. All three share the same constitutional text and founding problem but diverge sharply on ε, beneficiaries, and victims — they are linked here rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
