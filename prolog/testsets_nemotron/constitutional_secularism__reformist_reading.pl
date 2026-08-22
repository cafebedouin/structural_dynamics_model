% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__reformist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__reformist_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: Constitutional Secularism — Reformist Reading (Affirmative Duty to Eliminate Oppressive Religious Practices)
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the reformist_reading of the
 *   constitutional_secularism kernel. The reading asserts that the state
 *   bears an affirmative constitutional duty to eliminate religious practices
 *   that oppress marginalized groups — particularly scheduled castes and
 *   women — and that this duty supersedes claims of religious autonomy. This
 *   is the most interventionist of the three declared readings: it treats
 *   religious autonomy as a qualified right that yields when it functions as
 *   a vehicle for caste and gender oppression. The beneficiary set centers
 *   scheduled caste women and men, women in patriarchal communities, and
 *   Muslim women under personal law. The victim set comprises conservative
 *   religious authorities across multiple traditions who experience the
 *   constraint as an extraction of their communal self-governance. The
 *   constraint has hardened over the interval (1950-2010): early judicial
 *   restraint gave way to aggressive reform (Hindu Code Bills, Shah Bano,
 *   triple talaq abolition, Sabarimala), with rising extraction from
 *   religious autonomy and rising suppression of exit via judicial supremacy.
 *   Theater remains low — the reformist machinery produces real legal change
 *   — but creeps upward as performative declarations of 'social justice'
 *   occasionally substitute for enforcement in recalcitrant domains.
 *
 * KEY AGENTS:
 *   - State (agenda_setter): Institutional power, generational horizon, analytical exit — authors and enforces the affirmative duty through legislation and judicial review
 *   - Scheduled caste women and men (beneficiary): Powerless, biographical horizon, trapped exit — primary recipients of the reform's protection; no exit from caste oppression without state intervention
 *   - Women in patriarchal communities (beneficiary): Powerless/moderate, biographical horizon, constrained exit — benefit from reform but face community retaliation for claiming rights
 *   - Muslim women under personal law (beneficiary): Powerless, biographical horizon, trapped exit — direct beneficiaries of triple talaq abolition and maintenance rights reform
 *   - Hindu conservative orthodoxy (victim/payer): Organized, generational horizon, constrained exit — loses control over temple entry, caste norms, personal law; resists through political mobilization
 *   - Muslim conservative ulema (victim/payer): Organized, generational horizon, constrained exit — loses authority over personal law; resists through theological and political channels
 *   - Sikh orthodox institutions (victim/payer): Organized, generational horizon, constrained exit — contests state interference in gurdwara management and religious definition
 *   - Christian conservative denominations (victim/payer): Organized, generational horizon, constrained exit — resists reform of personal law and institutional autonomy
 *   - Tribal customary law authorities (victim/payer): Moderate, generational horizon, identity_locked exit — loses state recognition of customary practices that oppress women; exit means abandoning tribal identity
 *   - Supreme Court (agenda_setter/observer dual): Institutional, generational horizon, analytical exit — the primary enforcement engine; its jurisprudence constitutes the constraint's operational form
 *   - Parliament (agenda_setter): Institutional, biographical horizon, constrained exit — enacts reform legislation but responds to electoral pressures from victim communities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.82).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.78).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, snare).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "Constitutional Secularism — Reformist Reading (Affirmative Duty to Eliminate Oppressive Religious Practices)").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, '33db6e2a-38b8-43d4-bb0a-451b15e15ed4').
narrative_ontology:cs_kernel_codification('33db6e2a-38b8-43d4-bb0a-451b15e15ed4', formalized).
narrative_ontology:cs_authority_grounding('33db6e2a-38b8-43d4-bb0a-451b15e15ed4', lineage).
narrative_ontology:cs_interpretation_layer_present('33db6e2a-38b8-43d4-bb0a-451b15e15ed4').
narrative_ontology:cs_reading_relation('33db6e2a-38b8-43d4-bb0a-451b15e15ed4', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('33db6e2a-38b8-43d4-bb0a-451b15e15ed4', constitutional_secularism__principled_intervention_reading, influences).
narrative_ontology:cs_axiom('33db6e2a-38b8-43d4-bb0a-451b15e15ed4', foundational, state_affirmative_duty_eliminate_oppressive_practices).
narrative_ontology:cs_axiom_status(state_affirmative_duty_eliminate_oppressive_practices, holdable).
narrative_ontology:cs_axiom_grounding('33db6e2a-38b8-43d4-bb0a-451b15e15ed4', state_affirmative_duty_eliminate_oppressive_practices, deontological).
narrative_ontology:cs_axiom('33db6e2a-38b8-43d4-bb0a-451b15e15ed4', foundational, religious_autonomy_qualified_by_equality).
narrative_ontology:cs_axiom_status(religious_autonomy_qualified_by_equality, holdable).
narrative_ontology:cs_axiom_grounding('33db6e2a-38b8-43d4-bb0a-451b15e15ed4', religious_autonomy_qualified_by_equality, deontological).
narrative_ontology:cs_axiom('33db6e2a-38b8-43d4-bb0a-451b15e15ed4', secondary, constitutional_morality_supersedes_community_morality).
narrative_ontology:cs_axiom_status(constitutional_morality_supersedes_community_morality, holdable).
narrative_ontology:cs_axiom_grounding('33db6e2a-38b8-43d4-bb0a-451b15e15ed4', constitutional_morality_supersedes_community_morality, conventional).
narrative_ontology:cs_reference_frame('33db6e2a-38b8-43d4-bb0a-451b15e15ed4', constitutional_promise_of_equality_within_communities).
narrative_ontology:cs_drift_state('33db6e2a-38b8-43d4-bb0a-451b15e15ed4', contemporary_judicial_activism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('33db6e2a-38b8-43d4-bb0a-451b15e15ed4', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_caste_women).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_caste_men).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, women_in_patriarchal_communities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, muslim_women_under_personal_law).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, hindu_conservative_orthodoxy).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, muslim_conservative_ulema).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, sikh_orthodox_institutions).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, christian_conservative_denominations).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, tribal_customary_law_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and enforces the affirmative duty through legislation (Hindu Code Bills, Muslim Women Act, triple talaq law) and judicial review. The state controls the constraint's enforcement machinery and bears no cost of the extraction — it extracts autonomy from religious communities to redistribute protection to marginalized groups. Exit means constitutional amendment, which is blocked by basic structure doctrine.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, state, agenda_setter,
    institutional, generational, analytical, national).

% Primary beneficiaries of the reformist duty. Face intersecting caste and gender oppression within Hindu religious practices (temple exclusion, untouchability, denial of priesthood, marriage restrictions). Cannot exit caste oppression individually; the state's affirmative duty is their only structural exit. The constraint subsidizes their protection — they pay nothing and receive enforceable rights.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_caste_women, beneficiary,
    powerless, biographical, trapped, national).

% Beneficiaries of anti-untouchability enforcement, temple entry rights, and SC/ST atrocities legislation. Like scheduled caste women, they are structurally trapped in caste oppression without state intervention. The constraint operates as a subsidy for their dignity and access.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_caste_men, beneficiary,
    powerless, biographical, trapped, national).

% Women across religious communities who benefit from reform of personal laws (marriage, divorce, maintenance, inheritance). Exit is constrained: they can claim rights but face community ostracism, violence, and familial retaliation. The constraint provides legal remedies but cannot fully suppress the social cost of exercising them.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, women_in_patriarchal_communities, beneficiary,
    powerless, biographical, constrained, national).

% Direct beneficiaries of Shah Bano maintenance rights, triple talaq abolition, and proposed uniform civil code provisions. Under the pre-reform personal law regime, they had no exit from unilateral divorce and denied maintenance. The constraint's affirmative duty is their only structural protection — they are fully subsidized by the extraction from Muslim conservative authority.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, muslim_women_under_personal_law, beneficiary,
    powerless, biographical, trapped, national).

% Loses control over temple entry norms, caste-based priesthood restrictions, and Hindu personal law (reformed via Hindu Code Bills). Resists through political mobilization (Hindutva), judicial challenges, and social enforcement of traditional norms. Exit is constrained: they cannot leave the constitutional order, but can capture state power to reverse reforms (as seen in recent legislative pushes). The constraint extracts their communal autonomy and redistributes it to scheduled castes and women.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, hindu_conservative_orthodoxy, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__reformist_reading, hindu_conservative_orthodoxy, payer).

% Loses authority over Muslim personal law (triple talaq, maintenance, inheritance, marriage age). Resists through theological argument, political mobilization (AIMPLB), and claims of constitutional religious freedom (Article 25-26). Exit is constrained: they operate within the Indian constitutional order but seek autonomy through separate personal law. The constraint extracts their interpretive monopoly over Islamic law in India.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, muslim_conservative_ulema, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__reformist_reading, muslim_conservative_ulema, payer).

% Contests state interference in gurdwara management (SGPC control), definition of Sikh identity (Sehajdhari vs Amritdhari), and application of Hindu law to Sikhs. The reformist duty threatens their institutional autonomy. Exit is constrained: they have political representation in Punjab but cannot exit the Union's constitutional framework.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, sikh_orthodox_institutions, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__reformist_reading, sikh_orthodox_institutions, payer).

% Resists reform of Christian personal law (divorce, succession, adoption) and state regulation of religious institutions (educational autonomy, missionary activity). The affirmative duty to eliminate 'oppressive practices' threatens their canonical law. Exit is constrained: they operate within the constitutional order but claim minority institution protections (Article 30).
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, christian_conservative_denominations, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__reformist_reading, christian_conservative_denominations, payer).

% Loses state recognition of customary practices that oppress tribal women (inheritance, marriage, witch-hunting accusations). Their authority is fused with tribal identity — exit means abandoning the tribal legal order that constitutes their political distinctiveness. The constraint extracts their customary law autonomy in the name of gender equality, creating an identity-locked trap: accept reform and lose distinctiveness, or resist and face constitutional invalidation.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, tribal_customary_law_authorities, payer,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__reformist_reading, tribal_customary_law_authorities, payer).

% The primary enforcement engine of the reformist reading. Its jurisprudence (basic structure doctrine, essential practices test, constitutional morality) constitutes the constraint's operational form. The Court is both agenda_setter (authoring the duty) and observer (adjudicating its own precedents). It bears no extraction cost and controls the suppression machinery. Exit means constitutional amendment overturning its judgments — blocked by its own basic structure doctrine.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__reformist_reading, supreme_court, observer).

% Enacts reform legislation (Hindu Code Bills, Muslim Women Act, triple talaq law) but responds to electoral pressures from victim communities. The Court's basic structure doctrine constrains parliamentary reversal. Parliament is an agenda_setter with constrained exit — it can legislate but cannot amend the constraint's constitutional foundation.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, parliament, agenda_setter,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__reformist_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_secularism__reformist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a constitutional order where equality (Articles 14-15) supersedes religious autonomy (Articles 25-26) when the latter operates as a vehicle for caste and gender oppression. Solves the collective-action problem: marginalized individuals cannot overthrow communal oppression alone; the state's affirmative duty provides the enforcement capacity they lack.
% TRANSFER_FUNCTION: Transfers communal self-governance authority from conservative religious institutions (orthodox hierarchies, personal law boards, customary law bodies) to the state's judicial enforcement apparatus, which then distributes enforceable rights to marginalized group members (scheduled caste women, Muslim women, women in patriarchal communities). The transfer is non-consensual and asymmetric.
% ABSENT_VOICES: Marginalized members within conservative communities who oppose reform (e.g., conservative Muslim women who view triple talaq reform as anti-Islamic, scheduled caste individuals who reject state intervention as assimilationist). Their voices are excluded because the reformist reading defines 'the marginalized' as a monolithic beneficiary class. Also excluded: religious reformers from within communities who advocate endogenous change — the constraint's logic assumes state intervention is necessary, crowding out internal reform.
% DISAPPEARANCE_RATIONALE: If the affirmative duty vanished overnight, religious communities would immediately reassert autonomy over personal law and customary practices. Scheduled castes would lose temple entry and anti-untouchability enforcement; Muslim women would lose maintenance rights and triple talaq protection; tribal women would lose statutory protection against witch-hunting and discriminatory inheritance. The constitutional order would reorganize around religious autonomy as the primary norm, with equality as a secondary, non-enforceable aspiration.
% FOUNDING_PROBLEM: Religious practices in India (caste untouchability, Hindu personal law discrimination, Muslim personal law asymmetry, tribal customary oppression) functioned as vehicles for the systematic subordination of scheduled castes and women. The pre-constitutional status quo offered no remedy: communities were internally closed, and the colonial state declined intervention. The reformist reading was built to solve this by making the post-colonial state the affirmative guarantor of equality within religious communities.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by: (1) B.R. Ambedkar's constituent assembly speeches and 'Annihilation of Caste' — external to the benefiting religious authorities; (2) Shah Bano's own petition and subsequent Muslim women's movements (Bebaak Collective, BMMA) — beneficiaries attesting the problem persists; (3) Supreme Court judgments in Sabarimala, triple talaq, and SC/ST Act cases — the enforcing authority attesting the problem remains live; (4) National Crime Records Bureau data on caste atrocities and gender violence — empirical corroboration from outside the beneficiary set. No major political or religious authority outside the reformist coalition disputes that caste and gender oppression within religious communities persists.
narrative_ontology:disappearance_verdict(constitutional_secularism__reformist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__reformist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__reformist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(constitutional_secularism__reformist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__reformist_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__reformist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__reformist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.82 reflects the reformist reading's structural posture: the constraint affirmatively extracts autonomy from religious communities to redistribute protection to marginalized groups. This is not passive neutrality — it is active, directed extraction. Suppression 0.78 captures the constraint's reliance on judicial enforcement against resistant communities: exit is suppressed because religious communities cannot opt out of constitutional review, and legislative override is blocked by basic structure doctrine. Theater ratio 0.18 is low because the constraint produces real legal transformation (Hindu Code Bills, Shah Bano override, triple talaq abolition, Sabarimala entry), but the gradual rise reflects increasing performative invocation of 'constitutional morality' without enforcement in domains like child marriage and witch-hunting. Accessibility collapse 0.35 is moderate: alternatives (community self-reform, voluntary abandonment of oppressive practices) exist but are foreclosed by the constraint's logic — the state claims the duty, not the community. Resistance 0.72 is high: every major reform has faced organized resistance, political backlash, and constitutional challenges. The claimed type is snare because the reformist reading presents the intervention as protective (coordination for the vulnerable) while structurally operating as extraction from religious autonomy — the coordination function is real but the extraction is asymmetric, non-consensual, and actively enforced. The beneficiary/victim structure is cleanly separated across communities, not within persons (omega: beneficiary_victim_boundary_within_communities).
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (scheduled caste women, Muslim women under personal law), the constraint appears as a mountain-like protection — a constitutional guarantee that cannot be negotiated away. From the victim seats (conservative religious authorities), it appears as a snare — an extraction of communal self-governance enforced by a hostile state. From the Supreme Court seat (agenda_setter), it appears as a rope — a genuine coordination problem (how to reconcile equality and religious freedom) solved through judicial supremacy. The engine computes these per-seat types from the structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (scheduled caste women, women in patriarchal communities, Muslim women under personal law) are structurally powerless with trapped/constrained exit — they cannot leave the oppression without state intervention. The state (agenda_setter) is institutional with analytical exit — it bears no cost of the extraction and controls enforcement. Victims (conservative religious authorities across traditions) are organized with constrained exit — they can resist politically but cannot exit the constitutional order. Directionality derivation: beneficiaries d ≈ 0.1 (subsidized), state d ≈ 0.0 (full beneficiary of its own constraint), victims d ≈ 0.85 (full targets). The engine computes effective extraction χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (caste and gender oppression within religious communities) remains live — but the reformist reading's affirmative duty has hardened into a permanent extraction regime rather than a transitional correction. The constraint does not carry a sunset clause; its justification is the steady state of constitutional supremacy, not a transition. This prevents scaffold classification. The mandatrophy question is whether the affirmative duty has become self-perpetuating: does the state now need the duty more than the beneficiaries need the protection? The rising theater ratio suggests some performative drift, but low absolute theater and high real enforcement keep it in snare territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_frame,
    'This constraint is one reading (reformist_reading) of the contested kernel ''constitutional_secularism''. How does the sibling reading structure affect classification?',
    'Structural comparison across sibling readings: each reading instantiates a distinct constraint with its own ε, beneficiary/victim structure, and type. The kernel_id and reading_id are recorded here; sibling constraints are linked via network.affects_constraints and cs_structure.reading_relations.',
    'Confirms this story follows ε-invariance (DP-001): one reading = one constraint = one ε. Divergence from sibling readings is expected and diagnostic, not a defect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Commitment-system committer frame: this constraint is the reformist_reading of constitutional_secularism').

omega_variable(
    extraction_measurement_ambiguity,
    'Does the 0.82 extractiveness reflect the constraint''s inherent extraction from religious autonomy, or does it incorporate the reading''s view that the pre-existing religious practices were themselves extractive toward marginalized groups?',
    'Separate measurement: (a) extraction from religious communities'' autonomy under this reading''s enforcement regime; (b) extraction from marginalized groups under the pre-reform status quo. The engine reads ε as authored for the standing arrangement under contest (the reformist intervention), not the counterfactual.',
    'If ε conflates the two directions, the classification may misattribute extraction. A high ε on the intervention could be warranted extraction (removing a worse extractive structure). This ambiguity is irreducible without a two-constraint decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_measurement_ambiguity, conceptual, 'Whether ε measures the reform''s extraction from religious autonomy or the net extraction delta from the status quo').

omega_variable(
    beneficiary_victim_boundary_within_communities,
    'Are the declared beneficiaries and victims cleanly separable by community, or do intra-community power dynamics create overlapping identities (e.g., a conservative woman who opposes reform but would benefit from it)?',
    'Empirical study of reform reception within affected communities: survey and interview data on how marginalized members of conservative communities experience the reform, versus how community elites characterize it.',
    'If beneficiaries and victims overlap within the same persons, the constraint''s extraction profile is more complex than the current binary structure captures. Could shift classification toward tangled_rope if intra-community coordination is also present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_victim_boundary_within_communities, empirical, 'Intra-community overlap between beneficiary and victim identities').

omega_variable(
    enforcement_capacity_vs_declared_duty,
    'The reading declares an affirmative duty, but state enforcement capacity is uneven. Does the gap between declared duty and actual enforcement constitute theater, or does it reflect implementation friction?',
    'Track enforcement actions over time: cases filed, judgments rendered, compliance achieved. Compare declared affirmative duty (legal text) to enforcement throughput.',
    'If enforcement is systematically low relative to the declared duty, theater_ratio should rise and the constraint may drift toward piton (performative maintenance of a reformist posture). If enforcement is high but resisted, suppression rises and snare classification strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_declared_duty, empirical, 'Gap between affirmative duty declaration and enforcement throughput').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__reformist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cons_tr_t15, constitutional_secularism__reformist_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(cons_tr_t30, constitutional_secularism__reformist_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(cons_tr_t45, constitutional_secularism__reformist_reading, theater_ratio, 45, 0.15).
narrative_ontology:measurement(cons_tr_t60, constitutional_secularism__reformist_reading, theater_ratio, 60, 0.18).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__reformist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cons_be_t15, constitutional_secularism__reformist_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(cons_be_t30, constitutional_secularism__reformist_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(cons_be_t45, constitutional_secularism__reformist_reading, base_extractiveness, 45, 0.75).
narrative_ontology:measurement(cons_be_t60, constitutional_secularism__reformist_reading, base_extractiveness, 60, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__reformist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cons_su_t15, constitutional_secularism__reformist_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(cons_su_t30, constitutional_secularism__reformist_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(cons_su_t45, constitutional_secularism__reformist_reading, suppression_requirement, 45, 0.75).
narrative_ontology:measurement(cons_su_t60, constitutional_secularism__reformist_reading, suppression_requirement, 60, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__reformist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_secularism__reformist_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__principled_intervention_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, personal_law_reform_hindu_code_bills).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, personal_law_reform_shah_bano_override).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, personal_law_reform_triple_talaq_abolition).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, sabarimala_temple_entry).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, anti_caste_legislation_sc_st_act).

% DUAL FORMULATION NOTE:
% Constitutional secularism kernel decomposes into three readings with distinct ε and type profiles. This reformist_reading is the downstream-most (most extractive) constraint, influenced by the kernel's upstream commitments. The strict_neutrality_reading and principled_intervention_reading are sibling constraints in the same family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_secularism__reformist_reading, institutional, 0.05).
constraint_indexing:directionality_override(constitutional_secularism__reformist_reading, powerless, 0.1).
constraint_indexing:directionality_override(constitutional_secularism__reformist_reading, organized, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
