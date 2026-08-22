% ============================================================================
% CONSTRAINT STORY: family_law_authority__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: family_law_authority__muslim_shariat_reading
 *   human_readable: Nikah Under Shariat Governance - Muslim Personal Law Reading of Family Law Authority
 *   domain: comparative law/political theory/religious governance
 *
 * SUMMARY:
 *   In India, the marriage, dissolution, dower, and inheritance affairs of
 *   Muslim citizens are governed by the shariat as applied through the
 *   Shariat Application Act 1937, administered by qazis, dar-ul-qaza forums,
 *   and the interpretive authority of the ulama, with the All India Muslim
 *   Personal Law Board acting as the arrangement's public defender. Marriage
 *   (nikah) is structured as a contract between the parties with a mandatory
 *   dower; dissolution runs through talaq initiated by the husband, khula by
 *   mutual consent or adjudicator ruling, and judicial faskh in narrow cases;
 *   men may contract up to four marriages. The interval traces the
 *   arrangement's modern career: entrenchment under the 1937 Act (which
 *   itself displaced some regional customs that had been more favorable to
 *   women), a defensive hardening after the Shah Bano maintenance ruling and
 *   the 1986 legislative rollback, peak contestation through the Shayara Bano
 *   litigation, and the 2017 constitutional striking-down and 2019 statutory
 *   abolition of instantaneous triple talaq, which removed the sharpest edge
 *   while leaving the underlying asymmetries intact. The claim and the
 *   metrics are independent authored facts: the type is claimed as
 *   tangled_rope because the structure demonstrably performs both a
 *   coordination function and an extraction function through the same
 *   provisions; the metrics describe what the arrangement's operation has
 *   actually cost its governed population.
 *
 * KEY AGENTS:
 *   - muslim_male_spouses: net beneficiary (moderate/constrained) - hold dissolution initiative and plurality permission, pay bounded dower
 *   - muslim_female_spouses: primary target (moderate/identity_locked) - bear open-ended dissolution exposure, plurality risk, and gated exit; hold dower, maintenance, and inheritance entitlements
 *   - ulama_and_qazi_establishment: agenda setter (institutional/identity_locked) - fixes operative interpretation and collects adjudication authority
 *   - all_india_muslim_personal_law_board: agenda setter (institutional/constrained) - defends the arrangement's autonomy in courts and politics
 *   - reformist_islamic_scholars: excluded voice (moderate/identity_locked) - internal critics locked out of the adjudication seats
 *   - muslim_womens_rights_organizations: excluded voice (organized/identity_locked) - litigants and organizers outside the doctrine-setting rooms
 *   - family_courts_and_high_courts: observer (institutional/analytical) - applies and constitutionally tests the inherited doctrine
 *   - union_legislature: agenda setter at the statutory boundary (institutional/mobile) - frames and amends the arrangement's legal envelope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, 0.57).
domain_priors:suppression_score(family_law_authority__muslim_shariat_reading, 0.6).
domain_priors:theater_ratio(family_law_authority__muslim_shariat_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, extractiveness, 0.57).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__muslim_shariat_reading, "Nikah Under Shariat Governance - Muslim Personal Law Reading of Family Law Authority").
narrative_ontology:topic_domain(family_law_authority__muslim_shariat_reading, "comparative law/political theory/religious governance").

domain_priors:requires_active_enforcement(family_law_authority__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__muslim_shariat_reading, 'af8f0f8a-bce1-4ae4-8881-0eacdaed8d1f').
narrative_ontology:cs_kernel_codification('af8f0f8a-bce1-4ae4-8881-0eacdaed8d1f', fixed_text).
narrative_ontology:cs_authority_grounding('af8f0f8a-bce1-4ae4-8881-0eacdaed8d1f', lineage).
narrative_ontology:cs_interpretation_layer_present('af8f0f8a-bce1-4ae4-8881-0eacdaed8d1f').
narrative_ontology:cs_reading_relation('af8f0f8a-bce1-4ae4-8881-0eacdaed8d1f', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('af8f0f8a-bce1-4ae4-8881-0eacdaed8d1f', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('af8f0f8a-bce1-4ae4-8881-0eacdaed8d1f', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('af8f0f8a-bce1-4ae4-8881-0eacdaed8d1f', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('af8f0f8a-bce1-4ae4-8881-0eacdaed8d1f', foundational, marriage_is_contract_not_sacrament).
narrative_ontology:cs_axiom_status(marriage_is_contract_not_sacrament, holdable).
narrative_ontology:cs_axiom_grounding('af8f0f8a-bce1-4ae4-8881-0eacdaed8d1f', marriage_is_contract_not_sacrament, theological).
narrative_ontology:cs_axiom('af8f0f8a-bce1-4ae4-8881-0eacdaed8d1f', foundational, husband_holds_talaq_initiative).
narrative_ontology:cs_axiom_status(husband_holds_talaq_initiative, holdable).
narrative_ontology:cs_axiom_grounding('af8f0f8a-bce1-4ae4-8881-0eacdaed8d1f', husband_holds_talaq_initiative, theological).
narrative_ontology:cs_axiom('af8f0f8a-bce1-4ae4-8881-0eacdaed8d1f', secondary, mahr_obligatory_bridal_security).
narrative_ontology:cs_axiom_status(mahr_obligatory_bridal_security, holdable).
narrative_ontology:cs_axiom_grounding('af8f0f8a-bce1-4ae4-8881-0eacdaed8d1f', mahr_obligatory_bridal_security, theological).
narrative_ontology:cs_reference_frame('af8f0f8a-bce1-4ae4-8881-0eacdaed8d1f', classical_fiqh_marriage_framework).
narrative_ontology:cs_drift_state('af8f0f8a-bce1-4ae4-8881-0eacdaed8d1f', contemporary_post_shayara_bano, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('af8f0f8a-bce1-4ae4-8881-0eacdaed8d1f', '').
narrative_ontology:cs_kernel_id(family_law_authority__muslim_shariat_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, muslim_male_spouses).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, ulama_and_qazi_establishment).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, all_india_muslim_personal_law_board).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, muslim_female_spouses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, muslim_female_spouses).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, muslim_male_spouses).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Contract marriage by offer, acceptance, and witnesses, and owe a dower (mahr) to the wife plus maintenance during the waiting period after dissolution. Hold the right to initiate dissolution (talaq) without the wife's consent and may contract up to four simultaneous marriages. Paying mahr is bounded and often deferred until dissolution, which they control. Opting into civil marriage law instead carries community sanction and suspicion of abandoning the faith.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, muslim_male_spouses, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, muslim_male_spouses, payer).

% Receive a dower (frequently deferred to dissolution, when collection depends on the husband's cooperation), maintenance during the waiting period, and fixed inheritance shares. Bear the open-ended side of the arrangement: dissolution can be initiated unilaterally against them, a second marriage can be contracted over their objection, and their own exit (khula) requires the husband's consent or a religious adjudicator's ruling, typically at the price of returning the dower. Their marriage's validity is constituted by community recognition, so leaving the personal-law framework means forfeiting recognition of the marriage itself.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, muslim_female_spouses, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, muslim_female_spouses, beneficiary).

% Interpret the Quran and hadith for family matters, register marriages, sit in dar-ul-qaza and sharia adjudication forums, and issue fatwas on disputed dissolutions. Their standing, livelihood, and institutional continuity depend on the community continuing to bring marriage and divorce questions to religious adjudication rather than to civil forums. An individual scholar cannot exit the role without surrendering the transmission chain that constitutes his authority.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, ulama_and_qazi_establishment, agenda_setter,
    institutional, generational, identity_locked, global).

% Organized body that speaks for personal-law autonomy before courts and government, issued defenses of talaq practice in the Shayara Bano litigation, and publishes model nikahnamas. Its relevance depends on remaining the recognized interlocutor for Muslim family law; accepting wholesale statutory replacement would dissolve its reason to exist.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, all_india_muslim_personal_law_board, agenda_setter,
    institutional, generational, constrained, national).

% Argue from within the tradition that Quranic divorce is a staged process over three months rather than an instantaneous act, and read the higher objectives of the law (maqasid) as favoring protection of wives. They hold credentials in the same texts as the establishment but are largely shut out of the adjudication seats and board committees where operative interpretations are fixed. Remaining inside the faith community is constitutive of their argument's force, so exit is unthinkable.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, reformist_islamic_scholars, excluded,
    moderate, generational, identity_locked, global).

% Litigate and organize on behalf of women governed by personal law: the Shayara Bano petitioners, collectives drafting reformed nikahnamas banning instant divorce clauses, and campaigns for maintenance enforcement. They are not seated in the fiqh councils or board bodies where the operative rules are settled, yet their members remain bound by those rules and embedded in the communities that enforce them.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, muslim_womens_rights_organizations, excluded,
    organized, biographical, identity_locked, national).

% Adjudicate maintenance, dower recovery, and dissolution disputes arising under personal law while testing specific practices against constitutional guarantees. The Supreme Court in Shayara Bano held instantaneous triple talaq unconstitutional but declined to strike the wider framework. Courts apply the doctrine they inherit; they cannot rewrite it, only refuse enforcement of particular forms.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, family_courts_and_high_courts, observer,
    institutional, generational, analytical, national).

% Statutorily framed the modern arrangement (Shariat Application Act 1937, which made shariat govern Muslim family matters in place of divergent local customs) and amended its edges (Muslim Women Act 1986 after the Shah Bano ruling; Muslim Women (Protection of Rights on Marriage) Act 2019 criminalizing instant triple talaq). It moves the statutory boundary of the arrangement but leaves the interpretive core to the religious establishment.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, union_legislature, agenda_setter,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__muslim_shariat_reading, muslim_male_spouses).
narrative_ontology:fixing_cost_class(family_law_authority__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes marriage formation and dissolution inside the community: a recognizable contract form (offer, acceptance, witnesses, recorded dower), a defined waiting period before remarriage, fixed inheritance shares for widows and daughters, and a recognized forum for resolving marital disputes, so that unions are valid and dissolutions orderly without recourse to each party inventing terms.
% TRANSFER_FUNCTION: Moves dissolution initiative from both spouses to the husband alone; moves dower wealth from groom to bride on a schedule the husband largely controls; moves dispute-resolution authority and fees to the ulama and qazi forums; moves compliance from all parties to the interpreted corpus.
% ABSENT_VOICES: Women subject to unilateral dissolution had no seat in the fiqh councils and board bodies that fixed the operative rules; reformist scholars arguing from the same texts were marginalized from adjudication; the wives who petitioned in Shayara Bano entered the conversation only as litigants against the board, not as participants in doctrine-setting.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, Muslim marriages would re-form under the civil marriage statute or general contract law, dissolution would become symmetric and judicial, dower would be enforced as ordinary contract debt, and the qazi forums and the board would lose their caseload and standing; community dispute resolution, wedding practice, and inheritance administration would all reorganize within a generation.
% FOUNDING_PROBLEM: Seventh-century Arabian marriage custom had no ceiling on the number of wives, no mandatory payment to the bride, no waiting period protecting a divorced woman's paternity claims or subsistence, and no limits at all on abandonment. The Quranic provisions were built to fix exactly these: cap plurality at four with an equal-treatment condition, mandate the dower, institute the waiting period, and give women fixed inheritance shares where custom gave none.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of early Islamic law (working outside the benefiting parties) corroborate the founding problems and the genuinely protective character of the original provisions against seventh-century custom. Contemporary Muslim women's organizations attest a split verdict from the affected seat: the dower, maintenance, and inheritance provisions answer needs that remain live, while the unilateral dissolution power and plurality permission solve problems that modern civil courts and statutory maintenance law now handle. The ulama establishment and the board attest only the live-problem reading; no source outside the benefiting parties attests that the asymmetric powers specifically remain necessary.
narrative_ontology:disappearance_verdict(family_law_authority__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__muslim_shariat_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__muslim_shariat_reading, 0.57, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.57 at interval end: the dower, waiting-period, and inheritance provisions are genuine transfers toward wives, but the dissolution asymmetry, plurality permission, and gated khula are open-ended exposures whose expected cost falls almost entirely on one side. Suppression is 0.60: the arrangement persists through active machinery (community sanction, board mobilization, limited practical uptake of the civil-marriage alternative) rather than through participant preference alone. Theater ratio is 0.30: registration, dispute resolution, and dower recording are real work, but a visible share of establishment activity after 1985 consisted of resolutions, fatwa campaigns, and litigation posture defending the arrangement's autonomy rather than serving governed parties. The temporal series run on one shared grid (1937, 1960, 1985, 2000, 2017, 2024) with every tracked metric authored at every point. Suppression_requirement is tracked because enforcement capacity visibly changed twice: a build-up after Shah Bano (board consolidation, mobilization against state interference, peaking at 0.73 during the Shayara Bano contest) and a partial demobilization after the 2019 statute removed the arrangement's most indefensible enforcement object. Accessibility collapse is 0.50: exits exist (Special Marriage Act civil marriage, khula, judicial faskh) but each carries heavy social, religious, or evidentiary cost, so alternatives are degraded rather than eliminated. Resistance is 0.65: constitutional litigation, organized women's movements, and reformist scholarship produced actual doctrinal change in 2017-2019, which is more than a suppressed population usually manages.
 *
 * PERSPECTIVAL GAP:
 *   From the qazi's seat the arrangement is a transmitted order faithfully administered: the same clause that reads as a sword from the wife's seat reads as divinely allocated responsibility from his. From the board's seat the structure is minority self-governance under assimilation pressure, and every reform demand registers as an encroachment. From the male spouse's seat the obligations feel real but bounded - a dower sum, a maintenance period - while the prerogatives feel natural. From the wife's seat the identical provisions are an unbounded exposure she did not negotiate and cannot terminate. The engine computes these per-seat classifications from the structural data; the divergence between the agenda-setter seats and the payer seat is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Female spouses are declared victims and sit identity_locked, which places them near the full-target end: the arrangement's costs concentrate on them and exit would cost them the recognition of their own marriages. Male spouses are declared beneficiaries with a secondary payer position: they receive the dissolution initiative and plurality capacity and pay a bounded, often deferred dower, placing them near the beneficiary end. The ulama and the board are agenda setters whose authority and standing are produced by the arrangement's continuation; the derivation treats their institutional power and generational horizon accordingly. Courts and the legislature hold analytical or boundary-setting relationships with little personal stake in the flow either way. No directionality overrides are used: the beneficiary/victim declarations plus exit profiles produce the correct ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. A pure-snare reading would erase the documented protective core - the dower, the waiting period, and fixed inheritance shares were real transfers toward women against a custom that gave them nothing, and they continue to deliver value where civil enforcement fails. A pure-rope reading would erase the measured asymmetry: a coordination story in which one party holds the exit and the other holds the bill is not coordination. The tangled_rope claim keeps both facts load-bearing. The genealogy interview sharpens this: the founding problem splits into a live half (dower, maintenance, and inheritance provisions still answer needs that courts and markets underserve for poor women) and a dead-or-moribund half (the asymmetric powers were calibrated to a seventh-century environment of unlimited plurality and total abandonment, which statutory law has since closed). Because the status is contested rather than dead, this is not yet a piton candidate - but the 2017-2019 interventions show the dead half becoming legisgible, and further drift along that line would push the arrangement toward theatrical maintenance of provisions whose justification has lapsed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the muslim_shariat_reading of the family_law_authority kernel; which structural elements - dissolution symmetry, permitted plurality, the victim set, and the adjudicating authority - shift under each sibling reading, and do any sibling pairs stand in foreclosure rather than coexistence?',
    'Compile the four sibling stories and compare their authored structures; the engine computes foreclosure from axiom contradiction and grounding types across the family.',
    'No change to this file''s classification; determines the family-level comparison and which axes drive cross-reading divergence in victim sets and extraction profiles.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a five-reading kernel; sibling deltas carried by sibling files.').

omega_variable(
    protection_vs_extraction_attribution,
    'Do the dower, maintenance, and inheritance provisions operate as net economic protection for wives, or as consideration that offsets and thereby stabilizes the asymmetric dissolution powers?',
    'Longitudinal outcome comparison of divorced Muslim women under personal law versus comparable women married under the Special Marriage Act: asset retention, post-divorce income, and realized collection rates for deferred dower.',
    'If the provisions are net protection, effective extraction sits below the authored value and the coordination component dominates; if they function as consideration purchased with asymmetry, the authored value understates and the arrangement trends toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_vs_extraction_attribution, empirical, 'Whether the protective transfers and the asymmetric powers are separable or mutually stabilizing.').

omega_variable(
    khula_access_gap,
    'How accessible are khula and judicial faskh in practice relative to the formal right - what share of wives seeking dissolution obtain it without the husband''s consent, at what delay and cost?',
    'Grant rates, median delays, and fee-and-social-cost accounting from family court records and dar-ul-qaza registries, benchmarked against male-initiated talaq timelines.',
    'Near-symmetric practical access would pull effective extraction toward the symmetric band; blocked access confirms the dissolution asymmetry as the load-bearing extraction channel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(khula_access_gap, empirical, 'Formal versus realized symmetry of divorce access.').

omega_variable(
    polygyny_latent_capacity,
    'Does the permission of polygyny generate realized extraction through second marriages and inheritance fragmentation, or does it remain latent capacity whose threat value disciplines conduct within nominally monogamous marriages?',
    'Census and survey incidence of actual polygynous households among Muslims benchmarked against other communities, plus interview evidence on anticipatory behavior shaped by the permission.',
    'High realized incidence raises the extractiveness estimate; purely latent permission shifts the permission''s weight from extraction to suppression, as a standing threat that shapes conduct without being exercised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(polygyny_latent_capacity, empirical, 'Realized versus latent operation of the plurality permission.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression holding wives inside unwanted marriages structural (few affordable exits, weak dower enforcement, dependent children) or internalized (community pressure, piety-framed duty, fear of social death upon leaving the framework)?',
    'Post-exit trajectory studies of women who obtained khula, faskh, or civil remarriage: whether reported pressure and self-blame persist after the structural barrier is crossed.',
    'If substantially internalized, statutory remedies alone will underperform and effective suppression exceeds the structural measure; if structural, enforcement of maintenance and dower recovery bites directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of the suppression holding the payer seat in place.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__muslim_shariat_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1937, family_law_authority__muslim_shariat_reading, theater_ratio, 1937, 0.2).
narrative_ontology:measurement(fami_tr_t1960, family_law_authority__muslim_shariat_reading, theater_ratio, 1960, 0.22).
narrative_ontology:measurement(fami_tr_t1985, family_law_authority__muslim_shariat_reading, theater_ratio, 1985, 0.31).
narrative_ontology:measurement(fami_tr_t2000, family_law_authority__muslim_shariat_reading, theater_ratio, 2000, 0.34).
narrative_ontology:measurement(fami_tr_t2017, family_law_authority__muslim_shariat_reading, theater_ratio, 2017, 0.39).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__muslim_shariat_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(fami_be_t1937, family_law_authority__muslim_shariat_reading, base_extractiveness, 1937, 0.55).
narrative_ontology:measurement(fami_be_t1960, family_law_authority__muslim_shariat_reading, base_extractiveness, 1960, 0.56).
narrative_ontology:measurement(fami_be_t1985, family_law_authority__muslim_shariat_reading, base_extractiveness, 1985, 0.63).
narrative_ontology:measurement(fami_be_t2000, family_law_authority__muslim_shariat_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(fami_be_t2017, family_law_authority__muslim_shariat_reading, base_extractiveness, 2017, 0.61).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__muslim_shariat_reading, base_extractiveness, 2024, 0.57).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1937, family_law_authority__muslim_shariat_reading, suppression_requirement, 1937, 0.5).
narrative_ontology:measurement(fami_su_t1960, family_law_authority__muslim_shariat_reading, suppression_requirement, 1960, 0.53).
narrative_ontology:measurement(fami_su_t1985, family_law_authority__muslim_shariat_reading, suppression_requirement, 1985, 0.67).
narrative_ontology:measurement(fami_su_t2000, family_law_authority__muslim_shariat_reading, suppression_requirement, 2000, 0.69).
narrative_ontology:measurement(fami_su_t2017, family_law_authority__muslim_shariat_reading, suppression_requirement, 2017, 0.73).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__muslim_shariat_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__muslim_shariat_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'religious family law' decomposes into five structurally distinct arrangements, one per reading of the family_law_authority kernel. Each member authors its own epsilon over its own arrangement - sacramental indissolubility, dharmic samskara, community-preserving Zoroastrian regulation, revealed-corpus contract, and state-statute contract produce different victim sets, dissolution geometries, and authority seats, so no single story can carry the label without observable-dependent epsilon. This reading links to all four siblings; the upstream/downstream structure among them runs through the secular contractual reading, whose universality claim is pressured by the persistence of the four personal-law readings (and vice versa through uniform-code advocacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
