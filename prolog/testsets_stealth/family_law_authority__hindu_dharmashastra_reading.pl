% ============================================================================
% CONSTRAINT STORY: family_law_authority__hindu_dharmashastra_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__hindu_dharmashastra_reading, []).

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
 *   constraint_id: family_law_authority__hindu_dharmashastra_reading
 *   human_readable: Hindu Dharmashastra Marriage Regime — Sacramental Samskara Reading
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   Under the dharmashastra reading, marriage is a sacramental samskara —
 *   valid by saptapadi before the sacred fire, indissoluble across lifetimes,
 *   governed by smriti texts as absorbed through commentarial lineages
 *   (Mitakshara, Dayabhaga) and by recognized custom. The arrangement
 *   coordinates kinship formation and property devolution at civilizational
 *   scale while channeling labor, obedience, property, and adjudication rents
 *   asymmetrically: wives enter husbands' joint families without exit, widows
 *   are barred from remarriage, daughters stand outside coparcenary, and
 *   officiant lineages collect fees for rites only they may complete. The
 *   interval maps t=0 to approximately 1891 (the Age of Consent controversy)
 *   and t=60 to 1955 (the Hindu Marriage Act), tracing the regime's
 *   late-colonial arc. Claim and metrics are authored independently:
 *   claimed_type tangled_rope states my structural belief that genuine
 *   coordination and enforced asymmetry coexist in one arrangement; the
 *   metrics describe the operation as I read the record. Any divergence
 *   between claim and computed per-seat types is the datum, not an error. KEY
 *   AGENTS (by structural relationship): - brahmin_officiant_class: agenda
 *   setter and ritual collector (institutional/identity_locked) -
 *   orthodox_caste_assemblies: enforcement arm and secondary collector
 *   (organized/constrained) - joint_family_heads: primary material
 *   beneficiary (powerful/identity_locked) - married_women: primary target
 *   (powerless/trapped) - child_brides: acute target (powerless/trapped) -
 *   widows_denied_remarriage: acute target (powerless/trapped) -
 *   daughters_in_joint_family: target (powerless/constrained) -
 *   shudra_communities: excluded-and-bearing-costs (powerless/constrained) -
 *   shastric_reform_pandits: internal dissenters, repudiated
 *   (moderate/constrained) - anglo_hindu_colonial_courts: hybrid
 *   administrator (institutional/analytical) -
 *   legislative_reform_commissions: analytical observer
 *   (institutional/analytical)
 *
 * KEY AGENTS:
 *   - brahmin_officiant_class: agenda setter and ritual collector (institutional/identity_locked) — completes samskaras, transmits smriti, collects dakshina
 *   - orthodox_caste_assemblies: enforcement arm (organized/constrained) — adjudicate endogamy breaches, impose penance and boycott
 *   - joint_family_heads: primary material beneficiary (powerful/identity_locked) — karta controlling joint property and household labor
 *   - married_women: primary target (powerless/trapped) — transferred persons without dissolution remedy or independent support
 *   - child_brides: acute target (powerless/trapped) — married before consent capacity
 *   - widows_denied_remarriage: acute target (powerless/trapped) — permanent ascetic status, maintenance-only claims
 *   - daughters_in_joint_family: target (powerless/constrained) — excluded from coparcenary
 *   - shudra_communities: excluded-and-bearing-costs (powerless/constrained) — barred from Vedic rites and interpretive voice
 *   - shastric_reform_pandits: internal dissenters repudiated by orthodoxy (moderate/constrained)
 *   - anglo_hindu_colonial_courts: hybrid administrator elevating custom over text (institutional/analytical)
 *   - legislative_reform_commissions: analytical observer drafting codification (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, 0.62).
domain_priors:suppression_score(family_law_authority__hindu_dharmashastra_reading, 0.73).
domain_priors:theater_ratio(family_law_authority__hindu_dharmashastra_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hindu_dharmashastra_reading, "Hindu Dharmashastra Marriage Regime — Sacramental Samskara Reading").
narrative_ontology:topic_domain(family_law_authority__hindu_dharmashastra_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hindu_dharmashastra_reading, '277deac8-426d-4c0d-aeb9-439be8c5d869').
narrative_ontology:cs_kernel_codification('277deac8-426d-4c0d-aeb9-439be8c5d869', fixed_text).
narrative_ontology:cs_authority_grounding('277deac8-426d-4c0d-aeb9-439be8c5d869', lineage).
narrative_ontology:cs_interpretation_layer_present('277deac8-426d-4c0d-aeb9-439be8c5d869').
narrative_ontology:cs_reading_relation('277deac8-426d-4c0d-aeb9-439be8c5d869', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('277deac8-426d-4c0d-aeb9-439be8c5d869', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('277deac8-426d-4c0d-aeb9-439be8c5d869', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('277deac8-426d-4c0d-aeb9-439be8c5d869', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('277deac8-426d-4c0d-aeb9-439be8c5d869', foundational, marriage_seven_birth_indissoluble_sacrament).
narrative_ontology:cs_axiom_status(marriage_seven_birth_indissoluble_sacrament, holdable).
narrative_ontology:cs_axiom_grounding('277deac8-426d-4c0d-aeb9-439be8c5d869', marriage_seven_birth_indissoluble_sacrament, theological).
narrative_ontology:cs_axiom('277deac8-426d-4c0d-aeb9-439be8c5d869', foundational, varna_endogamy_preserves_dharma).
narrative_ontology:cs_axiom_status(varna_endogamy_preserves_dharma, holdable).
narrative_ontology:cs_axiom_grounding('277deac8-426d-4c0d-aeb9-439be8c5d869', varna_endogamy_preserves_dharma, theological).
narrative_ontology:cs_reference_frame('277deac8-426d-4c0d-aeb9-439be8c5d869', varnashrama_sacramental_ordainment).
narrative_ontology:cs_drift_state('277deac8-426d-4c0d-aeb9-439be8c5d869', eve_of_hindu_marriage_act, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('277deac8-426d-4c0d-aeb9-439be8c5d869', '').
narrative_ontology:cs_kernel_id(family_law_authority__hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, brahmin_officiant_class).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, orthodox_caste_assemblies).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, joint_family_heads).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, married_women).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, child_brides).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, widows_denied_remarriage).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, daughters_in_joint_family).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, shudra_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, joint_family_heads).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, varnashrama_dharma_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, smriti_commentarial_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Performs the samskara sequence — betrothal, fire rite, seven steps — without which elite marriages are ritually incomplete; transmits the smriti corpus through guru-lineages and adjudicates disputed points; receives dakshina fees and ritual precedence at every wedding. Leaving the vocation would mean abandoning the varna identity and textual mastery that constitute the class's standing; there is no exit that preserves who they are.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, brahmin_officiant_class, agenda_setter,
    institutional, generational, identity_locked, continental).

% Caste councils and dharma-sabhas that hear marriage disputes, fix penalties for endogamy breaches, prescribe prayaschitta penances, and organize social boycotts against non-compliant households. Their authority rests on community willingness to honor their rulings; they also collect fines and prestige from the adjudication role. Losing jurisdiction would dissolve the assembly's reason for meeting.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, orthodox_caste_assemblies, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(family_law_authority__hindu_dharmashastra_reading, orthodox_caste_assemblies, beneficiary).

% The karta of a joint household: controls ancestral property, arranges children's marriages, receives the incoming bride's labor and obedience, administers her separate property in practice, and allocates maintenance. Bears reciprocal duties — maintenance of dependents, ritual obligations — and loses honor if household discipline fails. His identity as household head is constituted by the arrangement he administers.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, joint_family_heads, beneficiary,
    powerful, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(family_law_authority__hindu_dharmashastra_reading, joint_family_heads, payer).

% Married by family arrangement, often before majority; moves into the husband's joint household where her labor, obedience, and fertility belong to his line. Cannot dissolve the bond regardless of treatment; her natal family's honor discourages return; economic survival runs through the marital household. Pativrata observance is the prescribed path to merit, including for a husband's welfare after death.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, married_women, payer,
    powerless, biographical, trapped, continental).

% Girls married before any capacity to consent, sometimes as small children; the timing of consummation was the live controversy of the 1891 Age of Consent fight following Phulmoni Dasi's death. Their agreement is given by guardians; exit is not a category that exists for them.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, child_brides, payer,
    powerless, biographical, trapped, continental).

% Once married, permanently marked: shaved head, plain dress, exclusion from auspicious rites, dependence on the husband's line for maintenance. Remarriage, where attempted, brought boycott of the whole family. The 1937 property act gave some a share, but status remained ascetic and dependent.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, widows_denied_remarriage, payer,
    powerless, biographical, trapped, continental).

% Grow up in a household whose property they will never share in; their marriages move them to another man's joint family. Stridhan gifts are their only recognized separate property, and even that is practically administered by others. Exit via marriage exchanges one joint family for another.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, daughters_in_joint_family, payer,
    powerless, biographical, constrained, continental).

% Barred from Vedic learning and from officiant roles in the higher samskaras; their marriages proceed through puranic rites with non-Vedic officiants, and their marriage disputes are judged by their own caste councils applying customary norms — under a textual order they were never permitted to read, let alone interpret. They bear the discipline of the marriage order without a voice in its terms.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, shudra_communities, excluded,
    powerless, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(family_law_authority__hindu_dharmashastra_reading, shudra_communities, payer).

% Tradition-trained scholars — Vidyasagar is the exemplar — who argue from the smriti corpus itself that widow remarriage and related reforms are textually licensed. Orthodoxy repudiates their credentials and counters with rival pandits and dharma-sabhas; their livelihoods and standing depend on the very institutions they challenge.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, shastric_reform_pandits, excluded,
    moderate, biographical, constrained, continental).

% Apply Anglo-Hindu law: the Privy Council holds that valid custom overrides written text, so the courts decide which customs bind which communities, effectively administering the arrangement's content. They take evidence from pandits, parties, and custom surveys; they marry no one, but their rulings reconfigure what the texts and customs require.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, anglo_hindu_colonial_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__hindu_dharmashastra_reading, anglo_hindu_colonial_courts, observer).

% The Age of Consent Committee and the Hindu Law Committee (Rau): gather testimony from every camp, draft codifications, and propose statutory replacement. Their seat is analytical — they can recommend but, within this interval, enact only fragments; the full code lands at the interval's edge.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, legislative_reform_commissions, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__hindu_dharmashastra_reading, joint_family_heads).
narrative_ontology:fixing_cost_class(family_law_authority__hindu_dharmashastra_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates household formation, lineage continuity, and property devolution across a stratified agrarian society: standardizes the samskara sequence, fixes permissible marriage pools through endogamy, routes inheritance through the male joint-family line, and binds two extended kin groups — not merely two individuals — into a durable alliance with ritual and economic obligations.
% TRANSFER_FUNCTION: Moves ritual fees and deference from marrying households to officiant lineages; moves labor, obedience, and reproductive capacity from wives to husbands' joint families; moves property control from daughters and widows to male coparceners; moves adjudication authority over marriage disputes to caste assemblies and pandit interpreters.
% ABSENT_VOICES: Women themselves held no seat in shastric interpretation — no woman authored the commentaries that defined their own status. Shudra communities were barred from Vedic learning and therefore from the textual conversation that governed their rites. Prospective inter-caste couples had no forum in which their unions could be defended. Where these voices appeared at all, it was as subjects of adjudication, never as participants in it.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, household formation, property devolution, the ritual economy of officiant fees, and caste boundary maintenance would all reorganize — which is precisely what the 1955-56 Hindu Code Acts demonstrated in slow motion: solemnization, succession, and maintenance were rebuilt on statutory foundations while the sacramental-hierarchical packaging was stripped away.
% FOUNDING_PROBLEM: An agrarian, stratified society needed reliable household formation, undisputed lineage continuity, and predictable property devolution across generations; it also needed to bind two extended kin groups into a durable alliance carrying ritual and economic obligations, and to mark the marriage as cosmically consequential so that families, not just individuals, held it together.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the post-1955 statutory regime (Hindu Marriage Act, Hindu Succession Act) addresses the same underlying problems — solemnization, succession, maintenance — while discarding the sacramental-hierarchical form, attesting that the coordination problem was real and independent of the extraction; comparative kinship scholarship documents the identical household-formation problem in similarly stratified agrarian societies; the Rau Committee evidence record and colonial custom surveys attest the problem's shape from administrative seats with no stake in officiant fees or household-head prerogatives.
narrative_ontology:disappearance_verdict(family_law_authority__hindu_dharmashastra_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__hindu_dharmashastra_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__hindu_dharmashastra_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__hindu_dharmashastra_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__hindu_dharmashastra_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__hindu_dharmashastra_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__hindu_dharmashastra_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62 is substantial but not total: the arrangement delivered real security and maintenance obligations alongside the transfers it imposed, and successive reforms (Age of Consent 1891, Child Marriage Restraint 1929, Hindu Women's Right to Property 1937) shaved specific extractions without touching the core. Suppression 0.73 is higher than extraction because persistence depended on actively closing exits — no dissolution remedy, boycott machinery against remarriage, ritual exclusion as sanction — not on participant preference. Suppression is authored as a raw structural property; only extractiveness gets scaled by directionality and scope downstream. Theater 0.40 reflects a rising share of performative defense: dharma-sabhas, pledge campaigns, and rival-pandit mobilizations defending the frame rather than performing its function. Accessibility collapse 0.52: conversion and anomalous regional customs left partial alternatives, so understanding the arrangement did not fully close the option space. Resistance 0.58: the widow-remarriage movement, Age of Consent agitation, and reformist pandits met the arrangement with organized opposition. The measurement series run on one shared grid (t=0,10,20,30,40,50,60) with all three metrics authored at every point; the rising suppression_requirement series is deliberate — the story traces enforcement hardening (boycott machinery maturing as compliance weakened), not merely extraction drift. Coalition note: the target class (women) was large but structurally disorganized — each woman's loyalty was routed toward her husband's household and her natal family's honor, cross-cutting caste ties prevented class-wide coalition, and the arrangement's design kept the people it disciplined from becoming a political class until late in the interval.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the officiant and household-head seats the arrangement is a sacred order they staff and sustain: duties, honor, and cosmic significance, with costs they acknowledge (maintenance burdens, ritual liability). From the wife, widow, and daughter seats the same structure operates as confinement without exit — the identical rule that constitutes the karta's identity constitutes their trap. The colonial courts occupy a third position: the arrangement is a rule-source to be administered, its content negotiable between text and custom. The engine derives these per-seat classifications from power, exit, and directional position; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place officiants, caste assemblies, and household heads near the beneficiary end (low d, damped or inverted effective extraction) — though the karta's secondary payer position (reciprocal maintenance duties) keeps him short of full subsidy. Victim declarations place married women, child brides, widows, and daughters near the full-target end; their trapped or constrained exit atoms push them further toward full target than mobile targets would sit. Shudra communities combine victim position with exclusion from the interpretive conversation — high d with no agenda-setting recourse. Courts and commissions hold no beneficiary or victim declaration; they sit near symmetric as administrators and analysts. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already produce the correct relationships, and the schema's override surface is reserved for cases the derivation cannot distinguish.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against mislabeling in both directions. Reading the arrangement as pure snare would erase the genuine coordination it performed — kinship formation, succession predictability, kin-group alliance — which is why the founding problem registers as live and why post-1955 statute rebuilt those functions in different form. Reading it as pure rope would erase the enforced asymmetry: the coordination story is real, but the same structure that coordinated also transferred labor, property, and adjudication rents from identifiable payers to identifiable collectors, and held by active enforcement. The R5 mismatch consumer reads founding_problem_status (live) against disappearance_verdict (world_rearranges): a live founding problem with a world that rearranges around the arrangement is the coherent cell — no zombie flag — because the problem persists even as the specific sacramental-hierarchical solution is replaced. Mandatrophy is not yet resolved for this reading: the arrangement had not outlived its function within the interval; it was being re-founded, not abandoned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the family_law_authority kernel (reading: hindu_dharmashastra_reading). What would each sibling reading change structurally if it displaced this one?',
    'Compare the compiled sibling stories directly: victim sets, exit options, and dissolution rules under muslim_shariat_reading (contract-plus-dower geometry), christian_canonical_reading (ecclesiastical indissolubility), parsi_zoroastrian_reading (community-council jurisdiction), and secular_contractual_reading (consent, divorce, equal property).',
    'If the secular_contractual reading displaced this one, the victim set contracts (dissolution and consent rights appear), the officiant fee stream disappears, caste assemblies lose adjudication jurisdiction, and the joint-family property routing unwinds — the arrangement''s effective extraction collapses toward the coordination floor. Displacement by the shariat reading would instead relocate the extraction geometry (dower, unilateral male dissolution) rather than remove it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: which kernel, which reading, what each sibling displacement would change.').

omega_variable(
    custom_vs_smriti_operative_layer,
    'How much of the operative arrangement is textual dharma versus regional custom, given the Privy Council''s elevation of valid custom above written smriti?',
    'Case-law coding of Anglo-Hindu reports separating custom-founded from text-founded holdings, combined with district-level custom surveys (Bhavani-type census material) mapping which communities lived under which rule-source.',
    'If custom dominates, a large share of the arrangement''s force belongs to the customary layer and the textual authority is partly theatrical; if text dominates, the commentarial lineage is the load-bearing collector and interpreter. The split changes which seat the extraction is attributed to.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custom_vs_smriti_operative_layer, empirical, 'Attribution of the arrangement''s operative force between smriti text and recognized custom.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (caste sanction, economic dependency, absence of dissolution remedy) or internalized (pativrata dharma making exit unthinkable even where options exist)?',
    'Post-reform exit trajectory: where statutory divorce and property rights arrived after 1955, track whether women''s behavior converged on the new options promptly or lagged persistently across cohorts; persistent lag indicates an internalized component surviving barrier removal.',
    'If substantially internalized, effective suppression exceeds the structural measure and outlives formal reform — the post-1955 statutory regime inherits a shadow arrangement that no repeal reaches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of suppression between external sanction and internalized duty.').

omega_variable(
    endogamy_coupling_separability,
    'Is caste endogamy part of this marriage-authority arrangement or a separate caste-purity arrangement coupled to it?',
    'Test whether sacramental validity and endogamy discipline come apart where one fails: regions with recognized anomalous customs (matrilineal inheritance, hypergamous grades) that retained full sacramental form while departing from strict endogamy.',
    'If separable, this reading''s epsilon drops toward the coordination floor and the endogamy arrangement carries the asymmetric discipline as its own story; if inseparable, the sacrament itself is the vehicle of caste discipline and the extraction stays here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(endogamy_coupling_separability, conceptual, 'Whether endogamy enforcement rides on the sacrament or constitutes an independent coupled arrangement.').

omega_variable(
    sacramental_frame_naturalness,
    'Is the sacramental-indissoluble frame a discovered cosmic order (as its theology presents it) or an authored authority structure benefiting identifiable classes?',
    'Historical philology of the samskara literature''s composition and patronage; comparison of indissolubility doctrine across strata, regions, and periods; documentation of who lost standing when the frame was publicly challenged.',
    'If authored, the frame''s natural-law presentation (unchangeable divine ordinance) is cover, and classification proceeds from the enforced asymmetries; if the frame was genuinely experienced as cosmic order by all parties including its targets, part of its stability is preference rather than coercion and the suppression measure overstates external force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_frame_naturalness, conceptual, 'Natural-law presentation versus constructed authority in the sacramental frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__hindu_dharmashastra_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(fami_tr_t50, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 50, 0.37).
narrative_ontology:measurement(fami_tr_t60, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(fami_be_t10, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 10, 0.69).
narrative_ontology:measurement(fami_be_t20, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(fami_be_t30, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(fami_be_t40, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(fami_be_t50, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 50, 0.63).
narrative_ontology:measurement(fami_be_t60, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(fami_su_t10, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(fami_su_t20, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(fami_su_t30, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(fami_su_t40, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(fami_su_t50, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(fami_su_t60, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 60, 0.73).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__hindu_dharmashastra_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (family_law_authority), five readings, five files. The colloquial label 'religious family law' conflates structurally distinct arrangements — sacrament (this reading, christian_canonical), contract-with-dower (muslim_shariat), community-institution (parsi_zoroastrian), and civil contract (secular_contractual) — each with its own epsilon, victim geometry, and enforcement machinery. This reading links to all four siblings; the upstream influence runs from this reading and its siblings jointly onto the secular_contractual reading, whose reach is bounded by personal-law persistence. Each file owns a single stable epsilon; no file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
