% ============================================================================
% CONSTRAINT STORY: family_law_authority__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__muslim_shariat_reading, []).

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
 *   constraint_id: family_law_authority__muslim_shariat_reading
 *   human_readable: Nikah Marriage Framework — Muslim Shariat Reading of Family Law Authority
 *   domain: legal/religious/political
 *
 * SUMMARY:
 *   Within India's personal-law system, the Muslim Shariat reading
 *   constitutes marriage as nikah — a civil contract whose terms (mahr dower,
 *   maintenance, dissolution procedure, polygyny permission) are fixed by
 *   Quranic injunction and hadith as transmitted through the fiqh schools,
 *   administered by ulama and qazis, and given state force by the Shariat
 *   Application Act 1937. The arrangement delivers genuine coordination —
 *   enforceable dower, maintenance obligations, and a dissolution pathway
 *   unavailable under the sacramental readings — while embedding asymmetric
 *   extraction: unilateral male talaq (instant triple talaq valid until the
 *   2017 Shayara Bano judgment and the 2019 statute), polygyny permitted to
 *   husbands only, and divorce access for wives mediated through husband
 *   consent or qazi/court intervention. KEY AGENTS (by structural
 *   relationship): husbands_under_nikah_contract: primary beneficiary
 *   (organized/constrained) — hold the talaq prerogative and polygyny option,
 *   owe mahr and maintenance; wives_under_nikah_contract: primary target
 *   (powerless/identity_locked) — hold mahr and maintenance claims, bear the
 *   dissolution asymmetry; ulama_and_qazi_judiciary and
 *   muslim_personal_law_board: agenda-setters (institutional/identity_locked)
 *   — interpret, adjudicate, and publicly defend the framework;
 *   patrilineal_kin_networks: beneficiary (organized/constrained) — contract
 *   marriages as alliances; muslim_women_reform_collectives: organized payers
 *   litigating against the asymmetry; constitutional_courts: analytical
 *   observer setting the framework's constitutional boundaries;
 *   women_jurists_excluded_from_fiqh_councils: excluded seat — trained in the
 *   tradition but absent from the councils that set its rules. This story is
 *   ONE reading of the family_law_authority kernel; the sibling readings
 *   (hindu_dharmashastra, christian_canonical, parsi_zoroastrian,
 *   secular_contractual) are separate constraints with their own epsilon
 *   values and victim structures. Claim and metrics are authored
 *   independently: the reading is CLAIMED as tangled_rope from the structural
 *   facts (real coordination function + real asymmetric extraction + active
 *   enforcement), and the metrics describe observed operation over 1975–2025.
 *
 * KEY AGENTS:
 *   - husbands_under_nikah_contract: primary beneficiary (organized/constrained) — hold talaq prerogative and polygyny option, owe mahr and maintenance
 *   - wives_under_nikah_contract: primary target (powerless/identity_locked) — hold mahr and maintenance claims, bear asymmetric divorce exposure
 *   - ulama_and_qazi_judiciary: agenda-setter (institutional/identity_locked) — interpret sources, adjudicate disputes, derive standing from the framework
 *   - muslim_personal_law_board: agenda-setter (institutional/identity_locked) — coordinates public and litigious defense of personal-law autonomy
 *   - patrilineal_kin_networks: beneficiary (organized/constrained) — contract marriages as alliances, benefit from framework stability
 *   - muslim_women_reform_collectives: organized payers — litigate and campaign for symmetric dissolution within an Islamic framework
 *   - constitutional_courts: analytical observer — adjudicate the framework against fundamental-rights guarantees
 *   - women_jurists_excluded_from_fiqh_councils: excluded seat — trained women jurists absent from the interpretive bodies that set the rules
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, 0.55).
domain_priors:suppression_score(family_law_authority__muslim_shariat_reading, 0.5).
domain_priors:theater_ratio(family_law_authority__muslim_shariat_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__muslim_shariat_reading, "Nikah Marriage Framework — Muslim Shariat Reading of Family Law Authority").
narrative_ontology:topic_domain(family_law_authority__muslim_shariat_reading, "legal/religious/political").

domain_priors:requires_active_enforcement(family_law_authority__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__muslim_shariat_reading, 'a990791a-04ce-410e-bca7-d3b26bf385ab').
narrative_ontology:cs_kernel_codification('a990791a-04ce-410e-bca7-d3b26bf385ab', fixed_text).
narrative_ontology:cs_authority_grounding('a990791a-04ce-410e-bca7-d3b26bf385ab', lineage).
narrative_ontology:cs_interpretation_layer_present('a990791a-04ce-410e-bca7-d3b26bf385ab').
narrative_ontology:cs_reading_relation('a990791a-04ce-410e-bca7-d3b26bf385ab', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('a990791a-04ce-410e-bca7-d3b26bf385ab', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('a990791a-04ce-410e-bca7-d3b26bf385ab', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('a990791a-04ce-410e-bca7-d3b26bf385ab', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('a990791a-04ce-410e-bca7-d3b26bf385ab', foundational, nikah_contractual_not_sacramental).
narrative_ontology:cs_axiom_status(nikah_contractual_not_sacramental, holdable).
narrative_ontology:cs_axiom_grounding('a990791a-04ce-410e-bca7-d3b26bf385ab', nikah_contractual_not_sacramental, theological).
narrative_ontology:cs_axiom('a990791a-04ce-410e-bca7-d3b26bf385ab', foundational, marriage_terms_fixed_by_revelation).
narrative_ontology:cs_axiom_status(marriage_terms_fixed_by_revelation, holdable).
narrative_ontology:cs_axiom_grounding('a990791a-04ce-410e-bca7-d3b26bf385ab', marriage_terms_fixed_by_revelation, theological).
narrative_ontology:cs_axiom('a990791a-04ce-410e-bca7-d3b26bf385ab', secondary, triple_talaq_valid_dissolution).
narrative_ontology:cs_axiom_status(triple_talaq_valid_dissolution, overridden).
narrative_ontology:cs_axiom_grounding('a990791a-04ce-410e-bca7-d3b26bf385ab', triple_talaq_valid_dissolution, theological).
narrative_ontology:cs_reference_frame('a990791a-04ce-410e-bca7-d3b26bf385ab', classical_fiqh_nikah_framework).
narrative_ontology:cs_drift_state('a990791a-04ce-410e-bca7-d3b26bf385ab', post_shayara_bano_india, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('a990791a-04ce-410e-bca7-d3b26bf385ab', '').
narrative_ontology:cs_kernel_id(family_law_authority__muslim_shariat_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, husbands_under_nikah_contract).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, ulama_and_qazi_judiciary).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, patrilineal_kin_networks).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, wives_under_nikah_contract).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, wives_under_nikah_contract).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, husbands_under_nikah_contract).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, muslim_women_reform_collectives).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, quranic_injunction_supremacy_in_marriage).
narrative_ontology:constraint_vindicates(family_law_authority__muslim_shariat_reading, hadith_transmission_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Men married under nikah hold the dissolution prerogative (talaq, pronounced with witnesses and procedure), may take up to four wives under the Quranic equal-treatment condition, and owe their wives mahr (dower held as the wife's exclusive property) and maintenance during marriage and iddat. Their position inside the arrangement is advantaged: they hold the exit key their wives lack. Leaving the framework itself — contracting a civil marriage under the Special Marriage Act instead — carries family and community cost, so most remain within it, but their standing inside it is strong.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, husbands_under_nikah_contract, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, husbands_under_nikah_contract, payer).

% Women married under nikah hold a dower claim that is their exclusive property and a maintenance right during marriage and iddat. They bear the arrangement's asymmetries: they can be divorced unilaterally by their husbands, their own divorce (khul') requires the husband's consent or the intervention of a qazi or court, and they are exposed to polygyny. Exit from the framework means leaving the faith community, family network, and social world that constitute their identity; mahr and maintenance claims are enforced unevenly, and post-divorce economic security is fragile.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, wives_under_nikah_contract, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, wives_under_nikah_contract, beneficiary).

% Scholars and judges trained in the fiqh schools interpret the Quran and hadith for family matters, register and adjudicate nikah disputes (including dar-ul-qaza forums running parallel to state courts), issue fatwas, and train the next generation of interpreters. Their standing, livelihood, and authority are constituted by the framework's continued governance of marriage; a wholesale shift of family law to state codification would dissolve the seat they occupy.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, ulama_and_qazi_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% A non-statutory coordinating body of ulama and community leaders that sets the community's public position on personal law. It litigated to defend triple talaq's validity before the Supreme Court, frames state intervention as an incursion on religious freedom, and organizes resistance to a uniform civil code. Its convening power depends on presenting itself as the community's authorized voice on the revealed sources.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, muslim_personal_law_board, agenda_setter,
    institutional, generational, identity_locked, national).

% Extended families contract marriages as alliances between households: guardians negotiate the match, the mahr amount, and the wedding obligations, and the legitimacy of offspring rides on the nikah's validity. The framework's stability serves their contracting practice and their control over marriage timing and partner selection; they bear the wedding costs and the alliance obligations that flow from each contract.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, patrilineal_kin_networks, beneficiary,
    organized, generational, constrained, regional).

% Organized women's groups — including the collectives that joined the Shayara Bano petition and campaign for a codified nikahnama — document cases of instant divorce and unpaid dower, provide legal aid to women seeking khul' or maintenance, and campaign for symmetric dissolution terms within an Islamic framework. They operate inside the community and bear backlash for their advocacy: social pressure, ostracism, and accusations of external alignment.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, muslim_women_reform_collectives, payer,
    organized, biographical, constrained, national).

% The High Courts and Supreme Court adjudicate challenges to personal-law practices against fundamental-rights guarantees. In Shayara Bano (2017) the Supreme Court held talaq-e-biddat void, and Parliament criminalized it in 2019. The courts do not administer the framework; they set its constitutional boundaries and can strike practices, which reshapes what the framework's administrators must accommodate.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Women trained in Islamic law who are largely absent from the fiqh academies, qazi councils, and the personal-law board where the framework's rules are interpreted and defended. They would contest the necessity of the dissolution asymmetry from within the tradition, but the interpretive seats are effectively closed to them; leaving the tradition to argue from outside would forfeit the standing that makes their argument count inside it.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, women_jurists_excluded_from_fiqh_councils, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__muslim_shariat_reading, husbands_under_nikah_contract).
narrative_ontology:fixing_cost_class(family_law_authority__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves household formation and lineage-legitimacy coordination for the community under religious law: it standardizes marriage contracting (offer, acceptance, witnesses), fixes the wife's financial claims (mahr as her exclusive property; maintenance during marriage and iddat), provides a bounded dissolution mechanism with waiting periods, and interfaces with the inheritance system.
% TRANSFER_FUNCTION: Moves mahr from the husband's side to the wife, where it remains her exclusive property; moves maintenance (nafaqa) from husband to wife during the marriage and iddat; moves adjudicative authority, fees, and deference to the ulama and qazi institutions; and — the asymmetry — concentrates unilateral dissolution discretion in the husband alone.
% ABSENT_VOICES: Women are largely absent from the fiqh councils and the personal-law board that set and defend the rules: trained women jurists would contest the asymmetry's necessity from within the tradition but hold no seat, and wives facing unilateral talaq had no seat in the board's Supreme Court defense of the practice. Their objections enter only through litigation and the reform collectives, not through the interpretive bodies themselves.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, marriage contracting, dower, maintenance, and divorce for the community would reorganize — under civil law, under a codified reformed nikah, or under competing religious authorities. The ulama's adjudication economy, the kin networks' contracting practice, and millions of existing and contingent mahr and maintenance claims would all require rearrangement; nothing about the arrangement is self-maintaining in its absence.
% FOUNDING_PROBLEM: Seventh-century Arabian context: regulating marriage alliances in a tribal order without state family courts; securing wives financially through mandatory dower in a setting where widowhood and arbitrary repudiation were destituting; and bounding pre-existing unbounded polygyny and repudiation (the four-wife cap with an equal-treatment condition; divorce subjected to procedure and waiting periods).
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by academic historians of early Islamic law (dower and waiting-period provisions as protections relative to pre-Islamic practice), by the reform collectives' documentation of post-divorce destitution (attesting the security problem persists in transformed form while the asymmetry no longer serves it), and by the Supreme Court's Shayara Bano analysis (the arbitration verse 4:35 read as contemplating third-party involvement in marital breakdown). No source outside the benefiting parties attests that instant triple talaq specifically was ever protective — the personal-law board itself called the practice sinful while defending its legal validity.
narrative_ontology:disappearance_verdict(family_law_authority__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__muslim_shariat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__muslim_shariat_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.55 at interval end: the dissolution asymmetry, polygyny permission, and unevenly enforced dower/maintenance are real costs concentrated on wives, but mahr and nafaqa are genuine reverse transfers and the 2019 statute removed the harshest term-form, so the arrangement is not purely extractive. The series shows a ratchet-and-reform arc: the 1986 maintenance rollback (after Shah Bano) raised extraction to its 0.68 peak; Danial Latifi (2001) and Shamim Ara (2002) began the decline; Shayara Bano (2017) and the 2019 Act brought it to 0.55. Suppression (0.50) is the community and fatwa enforcement layer plus the identity cost of exit; the suppression_requirement series traces enforcement capacity — rising through the deferential-court era to a 1995 peak (the 1986 Act was parliament actively enforcing the framework against a rights ruling), then decaying as constitutional review asserted. Theater (0.28) is low-moderate and rising: contracting, dower, and adjudication remain functional, but a growing share of the framework's public activity since 2017 defends institutional authority — the board's litigation and identity framing — rather than performing the coordination function. Accessibility_collapse (0.45): civil marriage under the Special Marriage Act remains a real alternative, so alternatives are only partly collapsed. Resistance (0.60): organized litigation, women's collectives, and internal fiqh dissent. All three series share one six-point time grid (T=0 corresponds to 1975, T=50 to 2025). Receipt surface: the asymmetry's direct gains (unilateral dissolution discretion, polygyny capacity) demonstrably accrue to the husbands' seat, so gain_flow names it; the ulama collect standing and adjudication income but the asymmetry's gains land with husbands. Fixing cost is prohibitive: the agenda-setters' authority is identity-constituted by revealed-term-fixity, and the state's political cost of full replacement (a uniform civil code) has so far outweighed the benefit — partial fixes such as the 2019 ban were extracted by litigation rather than chosen.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats should compute differently. From the ulama and board seats the arrangement is divinely ordered protection: mahr as the wife's secured property, bounded polygyny as a restriction on unlimited pre-Islamic practice, procedure-bound divorce as a restraint on caprice. From the wives' seat the same structure is asymmetric exit: the husband holds a unilateral key she lacks, and her own exit requires his consent or a third party's intervention. From the constitutional courts' seat it is a fundamental-rights conflict to be bounded case by case. The engine computes these per-seat classifications from the structural data — the authored tangled_rope claim is the whole-structure judgment and does not adjudicate the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Husbands sit near the beneficiary end (d low): they collect the asymmetry's gains and pay only the mahr/maintenance obligations, which damp but do not reverse their position. Wives sit near the full-target end: they bear the dissolution asymmetry and polygyny exposure while receiving mahr and maintenance. An explicit directionality override (power_atom powerless, d 0.85) is declared because the automatic derivation reads the wives' secondary beneficiary position (mahr, maintenance) and would damp d below the structural truth — enforcement failure on dower, post-1986 maintenance limits, and the exit asymmetry make them near-full targets in fact. The ulama and the personal-law board sit near the beneficiary end: they collect standing, income, and adjudication authority and bear none of the asymmetry's costs. Kin networks are beneficiaries of the contracting stability. The reform collectives are payers (organized, constrained exit — backlash costs them but they remain inside). Constitutional courts are the analytical seat with no directional stake. Scope is national: the enforcement instance is India's personal-law system, which amplifies effective extraction modestly relative to a local arrangement because verification of dower payment and maintenance compliance across a national population is harder.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problems — securing wives financially in a pre-state tribal economy and bounding unbounded repudiation and polygyny — were partly solved and partly transformed: dower and maintenance remain live protective transfers (the security problem they address persists in documented post-divorce destitution), while the instant-talaq element is a resolved mandatrophy whose protective rationale no one outside the benefiting parties defends and which the state formally superseded in 2017–2019. The classification prevents two mislabelings: reading the whole framework as pure extraction erases the real mahr and maintenance transfers that flow to wives and the dissolution pathway the sacramental readings lack; reading it as pure coordination erases the gender-asymmetric exit structure that requires active religious and social enforcement to hold. The framework is not a piton — its coordination function is live and its administrators profit from maintaining it — and the triple-talaq drift is captured as within-reading axiom override (see cs_structure) rather than as a whole-constraint type change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_delta,
    'This constraint instantiates the muslim_shariat_reading of the family_law_authority kernel (which sources fix marriage terms and which authorities interpret them). How would epsilon, the victim set, and the computed classification shift under a sibling reading — e.g., the secular_contractual_reading (terms fixed by party autonomy, symmetric dissolution, no polygyny) or the christian_canonical_reading (indissoluble sacrament, contractual dissolution removed entirely)?',
    'Author the sibling readings as separate constraint stories and compare engine-computed per-seat classifications across the family; the disagreement is located in the source-of-terms and dissolution-structure elements of the kernel, not in the existence of marriage regulation itself.',
    'The talaq asymmetry, polygyny permission, and mahr obligation are reading-specific deltas, not properties of family-law regulation as such; attributing them to the kernel would misclassify every sibling, and only the family comparison separates kernel-level from reading-level extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_delta, conceptual, 'Committer structure: one reading of the family_law_authority kernel; sibling readings change the victim set and the dissolution structure.').

omega_variable(
    asymmetry_textual_vs_interpretive,
    'Is the gender-asymmetric dissolution access required by the kernel sources themselves (the Quran''s divorce and arbitration verses, hadith on talaq) or an accretion of classical fiqh interpretation (procedure details, biddat validity, khul'' consent requirements)?',
    'Historical-critical fiqh scholarship: madhhab comparison, exegesis of the arbitration verse (4:35) and the waiting-period provisions, and the tradition''s own internal dissent, including jurists who held talaq-e-biddat impermissible long before the Indian litigation.',
    'If interpretive, the framework can reform within the reading — the interpretation layer absorbs the change without abandoning the kernel; if textual, the asymmetry is constitutive of this reading and reform requires a reading change toward a sibling or continued state override.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetry_textual_vs_interpretive, empirical, 'Whether the divorce asymmetry is kernel-level or interpretation-level — determines whether reform is internal to the reading or requires reading change.').

omega_variable(
    mahr_offset_effectiveness,
    'Does the dower-and-maintenance protective function actually offset the asymmetry''s costs for wives, or does enforcement failure (deferred or unpaid mahr, post-1986 maintenance limits, post-divorce destitution) make the protection nominal for a substantial share of women?',
    'Survey and court-record data on mahr payment rates and amounts relative to household income, maintenance recovery rates, and post-divorce economic outcomes for divorced Muslim women.',
    'If protection is largely nominal, the wives'' seat computes as near-full extraction and the coordination-function claim weakens toward pure extraction at that seat; if robust, the hybrid coordination-plus-asymmetry structure is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mahr_offset_effectiveness, empirical, 'Whether the framework''s protective transfers are effective or nominal — sets the extraction floor for the wives'' seat.').

omega_variable(
    wives_low_resistance_mechanism,
    'Is the low individual-level resistance observed among wives structural (economic dependency, no accessible forum, kin pressure) or internalized (acceptance fused with piety and community identity)?',
    'Post-exit trajectory: track rights-claiming among women who obtain khul'' or forum access through the reform collectives'' legal-aid clinics — if claiming rises once forum access exists, the suppression was structural; if acceptance persists after access, it is internalized.',
    'Structural suppression points to forum-access remedies; internalized suppression means the measured suppression understates the constraint''s effective hold and remedies must address identity, not only access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wives_low_resistance_mechanism, empirical, 'Structural versus internalized suppression mechanism for the wives'' seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__muslim_shariat_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(muslim_shariat_reading_tr_t0, family_law_authority__muslim_shariat_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(muslim_shariat_reading_tr_t0, observed).
narrative_ontology:measurement(muslim_shariat_reading_tr_t10, family_law_authority__muslim_shariat_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(muslim_shariat_reading_tr_t10, observed).
narrative_ontology:measurement(muslim_shariat_reading_tr_t20, family_law_authority__muslim_shariat_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(muslim_shariat_reading_tr_t20, observed).
narrative_ontology:measurement(muslim_shariat_reading_tr_t30, family_law_authority__muslim_shariat_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(muslim_shariat_reading_tr_t30, observed).
narrative_ontology:measurement(muslim_shariat_reading_tr_t40, family_law_authority__muslim_shariat_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement_basis(muslim_shariat_reading_tr_t40, observed).
narrative_ontology:measurement(muslim_shariat_reading_tr_t50, family_law_authority__muslim_shariat_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(muslim_shariat_reading_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(muslim_shariat_reading_be_t0, family_law_authority__muslim_shariat_reading, base_extractiveness, 0, 0.66).
narrative_ontology:measurement_basis(muslim_shariat_reading_be_t0, observed).
narrative_ontology:measurement(muslim_shariat_reading_be_t10, family_law_authority__muslim_shariat_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(muslim_shariat_reading_be_t10, observed).
narrative_ontology:measurement(muslim_shariat_reading_be_t20, family_law_authority__muslim_shariat_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(muslim_shariat_reading_be_t20, observed).
narrative_ontology:measurement(muslim_shariat_reading_be_t30, family_law_authority__muslim_shariat_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement_basis(muslim_shariat_reading_be_t30, observed).
narrative_ontology:measurement(muslim_shariat_reading_be_t40, family_law_authority__muslim_shariat_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement_basis(muslim_shariat_reading_be_t40, observed).
narrative_ontology:measurement(muslim_shariat_reading_be_t50, family_law_authority__muslim_shariat_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement_basis(muslim_shariat_reading_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(muslim_shariat_reading_su_t0, family_law_authority__muslim_shariat_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(muslim_shariat_reading_su_t0, observed).
narrative_ontology:measurement(muslim_shariat_reading_su_t10, family_law_authority__muslim_shariat_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement_basis(muslim_shariat_reading_su_t10, observed).
narrative_ontology:measurement(muslim_shariat_reading_su_t20, family_law_authority__muslim_shariat_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(muslim_shariat_reading_su_t20, observed).
narrative_ontology:measurement(muslim_shariat_reading_su_t30, family_law_authority__muslim_shariat_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement_basis(muslim_shariat_reading_su_t30, observed).
narrative_ontology:measurement(muslim_shariat_reading_su_t40, family_law_authority__muslim_shariat_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(muslim_shariat_reading_su_t40, observed).
narrative_ontology:measurement(muslim_shariat_reading_su_t50, family_law_authority__muslim_shariat_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement_basis(muslim_shariat_reading_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__muslim_shariat_reading, resource_allocation).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% The family_law_authority kernel decomposes into five reading-stories because the readings instantiate structurally distinct constraints with different epsilon values and victim sets: this reading fixes marriage terms by revelation (mahr, talaq, polygyny) under lineage-interpreted authority; the secular reading fixes terms by party autonomy under state authority; the canonical readings remove contractual dissolution entirely. The readings are linked as one constraint family: this reading coexists with the three religious siblings (parallel personal-law communities under one constitutional order, neither premise eliminating the other) and influences the secular reading — nikah's contractual form is invoked in uniform-civil-code debates as indigenous precedent for contract marriage, changing the secular reading's legitimacy environment without foreclosing it. Epsilon differs across the family: this reading's 0.55 reflects the dissolution asymmetry offset by dower and maintenance transfers; the secular reading's would reflect state-default terms with symmetric dissolution; the canonical readings' would reflect the absence of any contractual exit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__muslim_shariat_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
