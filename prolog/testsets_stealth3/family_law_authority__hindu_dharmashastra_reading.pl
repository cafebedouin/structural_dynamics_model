% ============================================================================
% CONSTRAINT STORY: family_law_authority__hindu_dharmashastra_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Sacramental Samskara Marriage Regime (Dharmashastra Reading)
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   Within the dharmashastra reading of family-law authority, a valid
 *   marriage is a sacramental samskara — one of the rites that complete a
 *   person's religious life — constituted by prescribed rites, gifts, and
 *   mantras rather than by a negotiable agreement, and binding across
 *   lifetimes. Governance runs through two channels: the textual tradition
 *   (the smriti corpus and its commentaries) read by learned pandits, and
 *   customary practice administered by caste and kin councils. The
 *   arrangement's structural features as this reading holds them: the bond is
 *   indissoluble (until the 1955 reform legislation admitted dissolution
 *   within Hindu personal law); legitimate unions follow varna and jati
 *   endogamy; property flows through agnatic coparcenary lines that exclude
 *   wives and daughters; and the wife enters as patni — the ritual partner
 *   who completes the husband's sacrificial capacity — rather than as an
 *   autonomous contracting party. Epsilon's referent is this standing
 *   sacramental arrangement as the reading itself holds it, assessed from the
 *   analytical generating seat; the claimed type is stated independently of
 *   the metrics, which describe the arrangement's observed operation across
 *   the interval 1800-1955.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_class: interpretive authority and ritual beneficiary (institutional / identity_locked) — reads the smriti corpus, officiates rites, collects dues; office fused with the tradition
 *   - male_lineage_heads: primary beneficiary (powerful / constrained) — controls agnatic property and household labor; standing rests on compliant alliance-making
 *   - jati_endogamy_councils: co-agenda-setter (organized / constrained) — adjudicates unions, punishes boundary crossings, keeps the marriage circle closed
 *   - married_wives: primary target (powerless / trapped) — lifelong bonded participation with no recognized exit
 *   - widows_denied_remarriage: extreme-position target (powerless / trapped) — visible renunciation that disciplines general compliance
 *   - pratiloma_union_participants: boundary-punishment target (powerless / trapped)
 *   - bride_natal_families: paying intermediary (moderate / constrained) — finances alliances, surrenders a daughter's labor
 *   - children_betrothed_in_minority: earliest cost-bearers (powerless / trapped) — contracted without consent
 *   - reformist_interpreters: excluded voice (moderate / mobile) — argues reinterpretation from within the texts without adjudicative standing
 *   - colonial_personal_law_administration: observer seat (institutional / analytical) — codifies and arbitrates what counts as compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, 0.68).
domain_priors:suppression_score(family_law_authority__hindu_dharmashastra_reading, 0.8).
domain_priors:theater_ratio(family_law_authority__hindu_dharmashastra_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hindu_dharmashastra_reading, "Sacramental Samskara Marriage Regime (Dharmashastra Reading)").
narrative_ontology:topic_domain(family_law_authority__hindu_dharmashastra_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hindu_dharmashastra_reading, '32deaae4-f345-4c79-9930-88c032dff638').
narrative_ontology:cs_kernel_codification('32deaae4-f345-4c79-9930-88c032dff638', fixed_text).
narrative_ontology:cs_authority_grounding('32deaae4-f345-4c79-9930-88c032dff638', lineage).
narrative_ontology:cs_interpretation_layer_present('32deaae4-f345-4c79-9930-88c032dff638').
narrative_ontology:cs_reading_relation('32deaae4-f345-4c79-9930-88c032dff638', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('32deaae4-f345-4c79-9930-88c032dff638', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('32deaae4-f345-4c79-9930-88c032dff638', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('32deaae4-f345-4c79-9930-88c032dff638', family_law_authority__secular_contractual_reading, forecloses).
narrative_ontology:cs_axiom('32deaae4-f345-4c79-9930-88c032dff638', foundational, marriage_indissoluble_sevenfold_sacrament).
narrative_ontology:cs_axiom_status(marriage_indissoluble_sevenfold_sacrament, overridden).
narrative_ontology:cs_axiom_grounding('32deaae4-f345-4c79-9930-88c032dff638', marriage_indissoluble_sevenfold_sacrament, theological).
narrative_ontology:cs_axiom('32deaae4-f345-4c79-9930-88c032dff638', foundational, varna_endogamy_orders_legitimate_union).
narrative_ontology:cs_axiom_status(varna_endogamy_orders_legitimate_union, holdable).
narrative_ontology:cs_axiom_grounding('32deaae4-f345-4c79-9930-88c032dff638', varna_endogamy_orders_legitimate_union, conventional).
narrative_ontology:cs_axiom('32deaae4-f345-4c79-9930-88c032dff638', secondary, wife_is_patni_ritual_partner_not_autonomous_party).
narrative_ontology:cs_axiom_status(wife_is_patni_ritual_partner_not_autonomous_party, holdable).
narrative_ontology:cs_axiom_grounding('32deaae4-f345-4c79-9930-88c032dff638', wife_is_patni_ritual_partner_not_autonomous_party, theological).
narrative_ontology:cs_reference_frame('32deaae4-f345-4c79-9930-88c032dff638', shastric_samskara_framework).
narrative_ontology:cs_drift_state('32deaae4-f345-4c79-9930-88c032dff638', eve_of_1955_codification, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('32deaae4-f345-4c79-9930-88c032dff638', '').
narrative_ontology:cs_kernel_id(family_law_authority__hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, male_lineage_heads).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, jati_endogamy_councils).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, married_wives).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, widows_denied_remarriage).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, pratiloma_union_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, bride_natal_families).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, children_betrothed_in_minority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the smriti corpus and preside over the wedding rites; their standing depends on being recognized as authoritative readers of the texts that define a valid union. They receive ceremonial dues and honoraria for officiating and for adjudicating questions of ritual validity. Leaving the tradition would forfeit the office, the lineage of training, and the livelihood built on both.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, brahmin_priestly_class, beneficiary,
    institutional, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(family_law_authority__hindu_dharmashastra_reading, brahmin_priestly_class, agenda_setter).

% Household patriarchs arrange marriages for their sons and daughters, hold ancestral property through the male line, and gain a ritually complete household plus the obligated labor of sons and daughters-in-law. Their standing among peers rests on marrying children properly within caste bounds. Stepping outside the norms costs them alliances and reputation among the same peers.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, male_lineage_heads, beneficiary,
    powerful, generational, constrained, continental).

% Caste assemblies hear marriage disputes, approve or refuse proposed unions, and punish boundary-crossing with fines, expiatory feasts, ritual exclusion, or expulsion. Keeping the community's marriage circle closed is the work that sustains their authority; each settled case replenishes the credibility the next ruling spends.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, jati_endogamy_councils, agenda_setter,
    organized, generational, constrained, regional).

% Married young into the husband's household, they move to his home, work under mother-in-law supervision, bear and raise children, and owe lifelong service; the union binds for life with no recognized way out, and returning to the natal home carries shame and economic precarity. Personal holdings are limited to whatever stridhan was gifted at marriage.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, married_wives, payer,
    powerless, biographical, trapped, regional).

% When husbands die, they may not remarry within communities observing the rule; they live on as dependents of in-laws or natal kin, often in plain dress with head shaved, and their visible renunciation stands before the whole community as a reminder of what deviation from the rules costs.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, widows_denied_remarriage, payer,
    powerless, biographical, trapped, regional).

% Couples whose union crosses varna lines in the disfavored direction lose caste standing, access to ritual services, and community membership, and their children inherit the degraded status. Each couple disciplined in this way serves as a public example deterring others who might attempt the same.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, pratiloma_union_participants, payer,
    powerless, immediate, trapped, regional).

% Parents of daughters assemble dowries and negotiate alliances within the permitted circle; they surrender a daughter's labor and presence to the husband's household and carry the cost of placing her respectably. Refusing the prevailing terms leaves daughters unplaced and the family exposed to gossip and diminished marriage prospects for younger siblings.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, bride_natal_families, payer,
    moderate, biographical, constrained, regional).

% Girls and boys contracted into marriage by guardians before they can consent; the girl typically joins the husband's household around puberty. They have no say at the moment the arrangement is fixed and no independent resources with which to revisit it afterward.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, children_betrothed_in_minority, payer,
    powerless, immediate, trapped, local).

% Scholars and reformers arguing from the textual tradition itself — citing older strata of the corpus, regional custom, or reason — for widow remarriage, later marriage ages, and limits on ritual authority. They publish, preach, and testify, but the recognized interpretive offices sit with the orthodox establishment, and their arguments gain traction mainly where courts or legislatures borrow them.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, reformist_interpreters, excluded,
    moderate, biographical, mobile, continental).

% Courts and law commissions that translate the textual tradition into administrable rules, survey customary practice, and decide which usages stand as authentic personal law. They neither marry under the rites nor officiate at them, but their codifications increasingly determine what counts as compliance for everyone else.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, colonial_personal_law_administration, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__hindu_dharmashastra_reading, male_lineage_heads).
narrative_ontology:fixing_cost_class(family_law_authority__hindu_dharmashastra_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves alliance formation and social reproduction under kinship economics: matches households within trusted networks, standardizes the rites that make unions ritually valid and socially recognized, transmits property down predictable succession lines, and organizes long-horizon care through the joint household.
% TRANSFER_FUNCTION: Moves women's labor, obedience, and reproductive capacity from natal to marital households; moves wealth through dowry and ceremonial gift predominantly toward the groom's side; moves ancestral property along male agnatic lines while excluding wives and daughters from coparcenary shares; moves ritual dues and interpretive fees to the priestly office; and moves social standing to men whose rites are complete.
% ABSENT_VOICES: Women governed by the texts had no seat in textual interpretation or council adjudication; children contracted in minority could not speak at all; reformist interpreters argued from outside the recognized offices and were heard chiefly where legislatures or courts borrowed their arguments. They sit outside the pandit assemblies and jati panchayats — in households, reform societies, and legislative commissions — without adjudicative standing in the tradition's own venues.
% DISAPPEARANCE_RATIONALE: Kinship alliance networks, succession and inheritance patterns, household authority hierarchies, ritual calendars, and caste boundary enforcement all presuppose the arrangement. Overnight removal would strand existing unions without recognized status rules, open succession disputes across every landed household, and strip away the machinery policing endogamy — the surrounding social order reorganizes around whatever replaces it.
% FOUNDING_PROBLEM: Stabilizing marriage alliance and property succession across generations in agrarian, caste-ordered kinship economies: guaranteeing that unions were ritually valid, that alliances were trustworthy, that estates passed intact down the male line, and that elders were cared for within the joint household.
% FOUNDING_PROBLEM_CORROBORATION: The orthodox beneficiary seats attest the problem as originally framed — alliance security, ritual validity, succession integrity — as still demanding the traditional form. Outside that set: witness testimony before nineteenth- and twentieth-century legislative commissions (women petitioners, medical officers on child marriage) attests both the harms the form produced and the persistence of the underlying care function; reformist philologists documented regional customs meeting alliance and care needs under far less coercive rules; and demographic and household-economy studies show the underlying problems continuing under altered forms. Independent attestation establishes the problem as real but its mandated solution as disputed.
narrative_ontology:disappearance_verdict(family_law_authority__hindu_dharmashastra_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__hindu_dharmashastra_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__hindu_dharmashastra_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__hindu_dharmashastra_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__hindu_dharmashastra_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.68) because the arrangement converts women's lifetime labor, reproductive capacity, and mobility into household and lineage assets while excluding them from ancestral property under agnatic coparcenary; dowry and ceremonial flows move wealth toward the groom's side; widowhood strips support entirely. It is not maximal because the joint-household architecture delivers real care, alliance insurance, and old-age support that seats across the structure drew on. Suppression (0.80) is a raw structural measure, deliberately unscaled: dissolution is simply unavailable within the framework, councils sanction boundary crossings with ostracism and ritual exclusion, and the pativrata ideal trains compliance from childhood — the enforcement burden sits on social and ritual machinery rather than a state apparatus for most of the interval. Accessibility collapse (0.78): once the framework is understood, the alternatives — leaving a marriage, crossing varna lines, holding ancestral property as a woman — are nearly unavailable inside the community; residual custom variation (matrilineal and bilateral pockets, regional schools) keeps it below unity. Resistance (0.55): sustained reform movements (widow-remarriage advocacy, age-of-consent campaigns, women's testimony to commissions) met orthodox counter-mobilization. The shared-grid temporal series traces a reform-and-ratchet cycle: each amelioration wave (1829; 1855-56; 1860/1891; 1929) lowers measured extraction modestly, enforcement adapts (dowry inflation, stricter council policing, deepened ritual emphasis), and extraction recovers within a generation, while theater_ratio climbs steadily as public orthodoxy is performed over a substantively eroding base. FNL alert: the tradition frames itself as identity coordination ('this is our dharma'); the identity_coordination leeway is warranted for genuine boundary maintenance, but the power-scope coupling concentrates costs on powerless agents (wives, widows, betrothed children) at regional scale inside a continental tradition — flagged for review rather than passed on the complexity offset.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats compute differently from the same structure. From the pandit's office and the lineage head's seat, the arrangement is a sacred order they steward: rites that complete persons, alliances that bind communities, continuity across generations — the coordination function is vivid and the costs are distributed and invisible from above. From a wife's or widow's seat, the same structure is a lifetime sentence: no recognized dissolution, supervised labor, property exclusion, social death as the price of deviation. Between nominally equal actors — a bride's natal family and a groom's family at the same caste standing — exit differs sharply: the groom's side accumulates assets and standing through the transaction while the bride's side pays it and loses a daughter's labor, so the same custom prices differently by seat. The engine computes these divergences from the declared structure; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The declared structure maps to directionality as follows. married_wives, widows_denied_remarriage, pratiloma_union_participants, and children_betrothed_in_minority sit nearest the full-target end (d approaching 1.0) — they bear the transfer and hold no exit, and trapped exit amplifies effective extraction further. male_lineage_heads sit near the beneficiary end: the arrangement subsidizes their property control and ritual completeness, and their constrained exit damps exposure. brahmin_priestly_class derive the lowest d — they collect dues and authority while bearing almost none of the arrangement's costs, and identity lock removes even career exit. jati_endogamy_councils sit low-to-moderate: they spend real enforcement effort but collect standing and marriage-market control. bride_natal_families occupy an intermediate-high position — genuine payers (dowry, surrendered labor) yet partial gainers through alliance capital, which the derivation reads from their payer role and constrained exit. No directionality overrides are used: the beneficiary/victim declarations together with power and exit atoms produce the correct relationships, and the internalized-suppression residue is routed to an omega rather than a d adjustment.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the arrangement as tangled_rope guards against two symmetrical errors. Reading it as pure extraction (a snare) erases the coordination seats across the structure actually relied on — alliance formation under uncertainty, care architecture in the joint household, ritual completion that conferred standing — and mispredicts why communities defended it for centuries. Reading it as pure coordination (a rope) hides the asymmetric transfer that made women's compliance compulsory and their exit impossible. On obsolescence: the founding problem (stable alliance and succession in agrarian kinship economies) remains live in contested form, so no resolved-mandatrophy boolean is authored; but the specific mechanism of sacramental indissolubility lost its mandate when the 1955 reform admitted dissolution — recorded in the overridden axiom and the severe, acknowledged practice-drift vector rather than in the boolean, which stays false because the broader mandate is disputed, not dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position_family_law_authority,
    'This constraint is one reading (hindu_dharmashastra_reading) of the contested kernel family_law_authority; how would instantiating a sibling reading change the structural data and the verdict?',
    'Generate and compare the sibling stories (muslim_shariat_reading, christian_canonical_reading, parsi_zoroastrian_reading, secular_contractual_reading): victim sets, exit structures, and epsilon differ by constitutive premise (sacrament versus contract), authority source (text-plus-custom, ecclesia, community law, state), and dissolution rules.',
    'Under the secular_contractual reading the wife becomes an autonomous contractor and dissolution is available, shrinking the victim set to coercion cases; under the shariat reading divorce exists but unilateral talaq concentrates exit asymmetry; under the canonical reading indissolubility persists but under ecclesiastical rather than textual-customary authority. Epsilon, directionality spread, and classification move accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position_family_law_authority, conceptual, 'Committer-frame routing: which reading of the marriage-authority kernel is instantiated and what siblings would change.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (non-recognition of dissolution, council ostracism, ritual exclusion) or internalized (pativrata self-concept and duty-fusion that make exit unthinkable even where custom permits)?',
    'Post-exit trajectory of women who left through migration, conversion, or customary loopholes: if self-subordinating patterns persist after the structural barrier is removed, a substantial share of suppression is internalized.',
    'If largely internalized, effective suppression exceeds the structural measure and survives formal reform — explaining why post-1955 divorce availability did not immediately translate into exit, and raising the true cost of the constraint to its targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in the pativrata socialization regime.').

omega_variable(
    endogamy_coordination_or_cover,
    'Is caste endogamy a genuine coordination function (alliance-capital pooling, network trust, risk-sharing among households) or extraction cover policing women''s marriages and property lines?',
    'Compare outcomes where endogamy enforcement relaxes — urbanizing cohorts, reformist communities, regions with bilateral custom: if alliance and care functions persist under relaxed boundaries, the coercive enforcement layer is separable from the coordination it claims to protect.',
    'If separable, the enforcement machinery is extraction riding on real coordination, weighting the verdict toward the extractive end of tangled_rope; if inseparable, part of the measured cost is the price of boundary maintenance itself, weighting it toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(endogamy_coordination_or_cover, conceptual, 'Whether the endogamy layer''s coordination and coercive components are structurally separable.').

omega_variable(
    amelioration_vs_enforcement_adaptation,
    'Does the mid-interval decline in base extractiveness reflect genuine amelioration (reform legislation, widening options) or enforcement adaptation that re-tightens extraction downstream (dowry inflation, stricter ostracism, deeper ritual emphasis)?',
    'Lagged-response analysis: extractiveness rebounds within roughly a generation of each reform wave in the series; trace whether reform cohorts show durable exit and property gains or renewed tighter control in the following decade.',
    'If adaptation dominates, the amelioration is transient and the regime is ratcheting (supporting the rising suppression series as the true dynamic); if gains are durable, the trajectory bends toward gradual retirement of the coercive layers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amelioration_vs_enforcement_adaptation, empirical, 'Whether reform-era extraction declines were durable or absorbed by enforcement adaptation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__hindu_dharmashastra_reading, 1800, 1955).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1800, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1800, 0.12).
narrative_ontology:measurement(fami_tr_t1845, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1845, 0.17).
narrative_ontology:measurement(fami_tr_t1890, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1890, 0.24).
narrative_ontology:measurement(fami_tr_t1925, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1925, 0.32).
narrative_ontology:measurement(fami_tr_t1940, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1940, 0.41).
narrative_ontology:measurement(fami_tr_t1955, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 1955, 0.46).

% Extraction over time
narrative_ontology:measurement(fami_be_t1800, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1800, 0.72).
narrative_ontology:measurement(fami_be_t1845, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1845, 0.69).
narrative_ontology:measurement(fami_be_t1890, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1890, 0.67).
narrative_ontology:measurement(fami_be_t1925, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1925, 0.65).
narrative_ontology:measurement(fami_be_t1940, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1940, 0.67).
narrative_ontology:measurement(fami_be_t1955, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 1955, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1800, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1800, 0.55).
narrative_ontology:measurement(fami_su_t1845, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1845, 0.59).
narrative_ontology:measurement(fami_su_t1890, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1890, 0.64).
narrative_ontology:measurement(fami_su_t1925, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1925, 0.71).
narrative_ontology:measurement(fami_su_t1940, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1940, 0.76).
narrative_ontology:measurement(fami_su_t1955, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 1955, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__hindu_dharmashastra_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% Constraint family: family_law_authority decomposes into five sibling readings (hindu_dharmashastra_reading, muslim_shariat_reading, christian_canonical_reading, parsi_zoroastrian_reading, secular_contractual_reading). The colloquial label 'religious marriage law' conflates five structurally distinct regimes with different constitutive premises, authority sources, dissolution mechanics, and party statuses; forcing them into one story would make epsilon observable-dependent, violating epsilon-invariance. Each member carries its own epsilon, beneficiary/victim sets, and claimed type; this member links all four siblings via affects_constraints. Upstream/downstream structure: the shariat and canonical readings historically influenced the codification environment this reading operated under, while the secular_contractual reading's instruments (state personal-law codes culminating in the 1955 act) supplied the formal vehicle for this reading's axiom-overriding event.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
