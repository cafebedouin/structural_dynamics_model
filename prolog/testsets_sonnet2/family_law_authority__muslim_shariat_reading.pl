% ============================================================================
% CONSTRAINT STORY: family_law_authority__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Nikah as Quranic-Hadith Governed Civil Contract (Muslim Personal Law, India)
 *   domain: religious_governance/family_law
 *
 * SUMMARY:
 *   This story reads Indian Muslim personal law's marital constraint — nikah
 *   as a civil contract governed by Quranic injunction and hadith,
 *   administered through the Muslim Personal Law Board and community qazi
 *   network — as its own kernel reading, distinct from the sacramental
 *   readings of Hindu, Christian, and Parsi personal law and from the secular
 *   contractual reading advanced by uniform civil code proponents. The ε
 *   value (0.58) and structural data describe the standing arrangement as it
 *   operates in India across 1937–2024, including the 1939 Dissolution of
 *   Muslim Marriages Act, the 1986 Muslim Women (Protection of Rights on
 *   Divorce) Act following Shah Bano, the 2017 Shayara Bano judgment, and the
 *   2019 Muslim Women (Protection of Rights on Marriage) Act criminalizing
 *   instant triple talaq — all read as evolution WITHIN this reading's
 *   kernel, not migration to a different reading. This is one component of a
 *   five-reading kernel family (family_law_authority); the sibling readings
 *   are separate constraint stories.
 *
 * KEY AGENTS:
 *   - husbands_under_unilateral_talaq: Primary beneficiary (moderate/mobile) — retains asymmetric dissolution and remarriage power
 *   - wives_subject_to_unilateral_talaq: Primary target (powerless/trapped) — bears the cost of asymmetric dissolution terms
 *   - muslim_personal_law_board: Agenda-setter (organized/arbitrage) — administers and defends the interpretive authority
 *   - qazi_and_darul_qaza_network: Administering institution (organized/arbitrage) — runs the parallel adjudicative machinery
 *   - indian_constitutional_courts: Analytical observer with real enforcement power (institutional/analytical) — reshapes enforceable content without abolishing the kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, 0.58).
domain_priors:suppression_score(family_law_authority__muslim_shariat_reading, 0.52).
domain_priors:theater_ratio(family_law_authority__muslim_shariat_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__muslim_shariat_reading, "Nikah as Quranic-Hadith Governed Civil Contract (Muslim Personal Law, India)").
narrative_ontology:topic_domain(family_law_authority__muslim_shariat_reading, "religious_governance/family_law").

domain_priors:requires_active_enforcement(family_law_authority__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__muslim_shariat_reading, 'fa6a2a77-9085-4f3f-b20b-b347e9db29c7').
narrative_ontology:cs_kernel_codification('fa6a2a77-9085-4f3f-b20b-b347e9db29c7', fixed_text).
narrative_ontology:cs_authority_grounding('fa6a2a77-9085-4f3f-b20b-b347e9db29c7', lineage).
narrative_ontology:cs_interpretation_layer_present('fa6a2a77-9085-4f3f-b20b-b347e9db29c7').
narrative_ontology:cs_reading_relation('fa6a2a77-9085-4f3f-b20b-b347e9db29c7', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa6a2a77-9085-4f3f-b20b-b347e9db29c7', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa6a2a77-9085-4f3f-b20b-b347e9db29c7', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa6a2a77-9085-4f3f-b20b-b347e9db29c7', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('fa6a2a77-9085-4f3f-b20b-b347e9db29c7', foundational, marriage_is_dissoluble_civil_contract_not_sacrament).
narrative_ontology:cs_axiom_status(marriage_is_dissoluble_civil_contract_not_sacrament, holdable).
narrative_ontology:cs_axiom_grounding('fa6a2a77-9085-4f3f-b20b-b347e9db29c7', marriage_is_dissoluble_civil_contract_not_sacrament, conventional).
narrative_ontology:cs_axiom('fa6a2a77-9085-4f3f-b20b-b347e9db29c7', foundational, unilateral_male_dissolution_prerogative_textually_grounded).
narrative_ontology:cs_axiom_status(unilateral_male_dissolution_prerogative_textually_grounded, holdable).
narrative_ontology:cs_axiom_grounding('fa6a2a77-9085-4f3f-b20b-b347e9db29c7', unilateral_male_dissolution_prerogative_textually_grounded, theological).
narrative_ontology:cs_axiom('fa6a2a77-9085-4f3f-b20b-b347e9db29c7', secondary, mahr_constitutes_sufficient_contractual_consideration_for_asymmetry).
narrative_ontology:cs_axiom_status(mahr_constitutes_sufficient_contractual_consideration_for_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('fa6a2a77-9085-4f3f-b20b-b347e9db29c7', mahr_constitutes_sufficient_contractual_consideration_for_asymmetry, conventional).
narrative_ontology:cs_reference_frame('fa6a2a77-9085-4f3f-b20b-b347e9db29c7', classical_hanafi_fiqh_marital_jurisprudence).
narrative_ontology:cs_drift_state('fa6a2a77-9085-4f3f-b20b-b347e9db29c7', post_2019_triple_talaq_criminalization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fa6a2a77-9085-4f3f-b20b-b347e9db29c7', '').
narrative_ontology:cs_kernel_id(family_law_authority__muslim_shariat_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, husbands_under_unilateral_talaq).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, all_male_dependents_under_polygyny_provision).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, muslim_personal_law_board).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, qazi_and_darul_qaza_network).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, wives_subject_to_unilateral_talaq).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, co_wives_in_polygynous_households).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, divorced_women_denied_maintenance_beyond_iddat).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically able to dissolve a marriage by unilateral pronouncement (talaq-e-biddat before the 2019 ban, and still via other talaq forms) without judicial process or the wife's consent. Retain lawful access to polygyny (up to four wives) subject only to the capacity-to-maintain condition, which is rarely enforced. Bear the mahr obligation but can often negotiate or defer it.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, husbands_under_unilateral_talaq, beneficiary,
    moderate, biographical, mobile, national).

% Can be divorced by pronouncement outside a court, historically with minimal notice or process, losing marital status, residence, and maintenance beyond the iddat period. Access to khula (wife-initiated dissolution) exists in doctrine but is harder to exercise in practice, frequently requiring either husband consent or protracted qazi/court proceedings. Exit from the marriage is available in name but the terms of exit are set unilaterally by the husband.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, wives_subject_to_unilateral_talaq, payer,
    powerless, biographical, trapped, national).

% Share a husband's attention, income, and household resources among multiple wives under a lawful polygyny arrangement they typically did not consent to in advance. Individually cannot compel equal treatment through the personal law framework; recourse is largely social or through general contract/maintenance law rather than the marital constraint itself.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, co_wives_in_polygynous_households, payer,
    powerless, biographical, constrained, national).

% Under the classical reading, maintenance obligation ends at the iddat period (roughly three menstrual cycles), after which the divorced wife has no continuing claim on the former husband beyond any settled mahr and iddat-period support (a position long contested in Indian courts, notably Shah Bano and the subsequent 1986 Muslim Women Act). Left to natal family, remarriage, or the state welfare system for ongoing support.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, divorced_women_denied_maintenance_beyond_iddat, payer,
    powerless, biographical, constrained, national).

% Represents itself as custodian and interpreter of correct shariat practice, lobbies against legislative reform (including opposing the codification of maintenance rights and the criminalization of triple talaq), and issues guidance to community qazis. Positioned to reframe reform proposals as attacks on religious freedom, giving it leverage independent of any individual marriage's outcome.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, muslim_personal_law_board, agenda_setter,
    organized, generational, arbitrage, national).

% Administers nikah registration, talaq documentation, khula petitions, and mahr disputes through parallel community tribunals operating alongside (and sometimes in tension with) the formal judiciary. Draws authority and standing from being the recognized interpreter of the Quranic-hadith framework for the community it serves.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, qazi_and_darul_qaza_network, agenda_setter,
    organized, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, qazi_and_darul_qaza_network, beneficiary).

% Adjudicate the boundary between personal law autonomy and constitutional guarantees of equality and dignity (Shah Bano 1985, Shayara Bano 2017, the 2019 Muslim Women (Protection of Rights on Marriage) Act criminalizing instant triple talaq). Their rulings reshape the enforceable content of the constraint without abolishing the underlying kernel reading.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, indian_constitutional_courts, observer,
    institutional, generational, analytical, national).

% Groups such as the Bharatiya Muslim Mahila Andolan have petitioned courts and legislatures for reform of talaq, mahr enforcement, and maintenance rules from within an Islamic framework (arguing the classical reading itself misapplies Quranic intent), but are not the recognized interpretive authority and must route their claims through courts or public advocacy rather than through the personal law board's own channels.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, reformist_muslim_womens_organizations, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__muslim_shariat_reading, diffuse).
narrative_ontology:fixing_cost_class(family_law_authority__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, religiously legitimate, community-recognized framework for entering, documenting, and dissolving marriage — mahr, witnesses, registration, and dissolution procedures are settled once by reference to a shared textual authority rather than negotiated de novo by each couple, and the same framework is recognized across the community regardless of which qazi or region administers it.
% TRANSFER_FUNCTION: Moves control over the timing, terms, and financial consequences of marital dissolution disproportionately toward husbands: unilateral talaq access, the capacity-gated but permissive polygyny rule, and the time-limited maintenance obligation together transfer economic security and relationship-continuity risk from husbands to wives and co-wives, while mahr functions as a partial (often under-enforced) offsetting transfer toward wives at the point of contract or dissolution.
% ABSENT_VOICES: Reformist Muslim women's organizations and individual wives seeking to renegotiate the classical terms (particularly on maintenance duration and unilateral talaq) are not the recognized interpretive authority; their objections reach the system only through constitutional litigation or legislative advocacy external to the personal law board's own structure, which frames such advocacy as external interference rather than internal reform.
% DISAPPEARANCE_RATIONALE: If this reading's authority collapsed overnight, marriage, divorce, and maintenance for Indian Muslims would default to whatever secular or codified statute filled the gap; qazi networks would lose their adjudicative role, the personal law board would lose its primary reason for organized existence, and millions of existing marriages, mahr agreements, and pending talaq/khula proceedings would need a new governing framework — the disappearance is not merely symbolic.
% FOUNDING_PROBLEM: To provide marriage, dissolution, and inheritance rules for the Muslim community that are religiously legitimate (traceable to Quranic text and hadith) rather than imposed by a colonial or secular state indifferent or hostile to Islamic practice, and to give community-recognized dispute resolution independent of a potentially discriminatory formal judiciary.
% FOUNDING_PROBLEM_CORROBORATION: The Muslim Personal Law Board and qazi network attest the founding problem remains live — protecting religious autonomy against state encroachment. Indian constitutional courts (Shah Bano, Shayara Bano) and reformist Muslim women's organizations, both outside the board's own constituency of benefit, attest that the specific gender-asymmetric mechanisms (instant unilateral talaq, unbounded maintenance cutoff) have outlived any textual necessity and now function primarily to preserve unilateral male dissolution power; the 2019 legislative criminalization of instant triple talaq is itself independent corroboration that a state body found the arrangement's specific mechanism, not merely its religious framing, unjustifiable.
narrative_ontology:disappearance_verdict(family_law_authority__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__muslim_shariat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__muslim_shariat_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) is substantial but not extreme: mahr provides a genuine, if inconsistently enforced, offsetting transfer to wives, and khula and judicial divorce routes exist in doctrine, so the constraint is not pure extraction. Suppression (0.52) reflects that exit from the marriage itself is not blocked — divorce is achievable — but the TERMS of exit are set asymmetrically and enforcement of wife-favorable provisions (mahr collection, maintenance beyond iddat) is comparatively weak. Accessibility collapse is moderate (0.45): once a woman understands her legal position, some alternatives (secular court, the 2019 criminal remedy for instant talaq, khula) are genuinely available, distinguishing this from a fully collapsed structure. Resistance is comparatively high (0.62) precisely because reformist litigation, legislative amendment, and internal reform movements have persistently contested the arrangement across the measured interval — this is a constraint that has been fought over, not passively accepted.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (personal law board, qazi network) this looks like a rope — a genuine, religiously grounded coordination solution protecting community autonomy from state overreach. From the payer seat (wives under unilateral talaq, especially pre-2019) the same structure computes as extractive: a legally sanctioned asymmetry enforced by community institutions with limited outside recourse. The engine's tangled_rope computation captures exactly this: a real coordination function (settled, legitimate, portable marriage law for a religious minority) coexisting with asymmetric extraction (gender-differentiated dissolution power) sustained by active institutional enforcement (personal law board advocacy, qazi adjudication, resistance to codification).
 *
 * DIRECTIONALITY LOGIC:
 *   Husbands and the administering institutions (personal law board, qazi network) sit near the beneficiary end of directionality: they retain procedural control (unilateral talaq access, polygyny option) and/or institutional standing (interpretive authority, adjudicative role) without bearing the asymmetric costs. Wives, co-wives, and post-iddat divorced women sit near the target end: they bear the asymmetric dissolution terms, resource-sharing costs, and truncated maintenance without a comparable unilateral remedy. Mahr is deliberately not treated as fully offsetting this asymmetry in the extractiveness score — it is a real but partial and inconsistently enforced counter-transfer, not full compensation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — religiously legitimate family law independent of a potentially hostile or indifferent state — remains partly live (minority religious autonomy is a continuing constitutional value in India) even as the specific gender-asymmetric mechanisms have been substantially eroded by litigation and legislation (1986 Act's later dilution, Shayara Bano, the 2019 criminalization of instant triple talaq). This is precisely the tangled_rope case the framework is built to distinguish from either a pure snare (which would require treating the entire coordination function as pretextual) or a pure rope (which would require ignoring the asymmetric cost structure): the coordination function is real and the extraction is real, and they are bound together in the same institutional structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_versus_interpretive_extraction,
    'Is the gender-asymmetric structure (unilateral talaq access, capacity-gated polygyny, time-limited maintenance) a direct requirement of the Quranic text and authoritative hadith, or a product of centuries of interpretive tradition (fiqh) that could be revised within the same textual sources — as reformist Muslim women''s organizations and some contemporary Islamic scholars argue?',
    'Comparative jurisprudential analysis across Islamic legal schools (Hanafi, Shafi''i, Maliki, Hanbali, Shia jurisprudence) and jurisdictions that have codified more gender-symmetric provisions (e.g., Tunisia, Morocco''s Moudawana reforms) within an explicitly Islamic legal framework, testing whether textual constraints or interpretive tradition is the binding element.',
    'If the asymmetry is interpretive rather than textually mandated, the constraint''s extraction is closer to constructed institutional practice riding on textual legitimacy (raising ε toward snare-adjacent) than to an irreducible feature of the reading''s own kernel; if textually mandated, the coordination/extraction bundle is more tightly fused and the tangled_rope classification is more stable across potential reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_versus_interpretive_extraction, conceptual, 'Whether gender asymmetry is textually required or an interpretive layer that could be revised within the same religious framework.').

omega_variable(
    state_versus_community_authority_boundary,
    'Where does legitimate state constitutional oversight of a religious minority''s personal law end and impermissible interference with religious autonomy begin — and does the answer shift the classification from tangled_rope toward scaffold (if the current asymmetric structure is genuinely transitional pending further codified reform) or keep it a stable hybrid?',
    'Track whether the post-2019 legal landscape (criminalized instant triple talaq, ongoing litigation on maintenance and mahr enforcement) represents a bounded, terminating reform trajectory (which would support scaffold characteristics) or a permanent, contested equilibrium between constitutional courts and personal law board (which supports the tangled_rope reading used here).',
    'A scaffold reading would require declaring a sunset condition this story does not currently assert; absent an explicit declared transition endpoint, tangled_rope is the more defensible claim, but future codification (e.g., a uniform civil code or comprehensive Muslim family law reform act) could resolve this toward either rope (if asymmetries are removed while coordination is preserved) or force decomposition into a new constraint story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_versus_community_authority_boundary, conceptual, 'Whether the current constitutional-court/personal-law-board contest is a stable hybrid or a transitional scaffold toward codified reform.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the correct framing of this constraint ''the Quranic-hadith textual kernel as such'' (which would make it closer to a mountain from within the tradition, since the text is treated as fixed and beyond human authorship) or ''the institutional authority claiming to interpret that kernel'' (which makes the constructed, contestable, revisable character of the personal law board''s specific rulings the operative object)? The two framings would yield different classifications: the textual-kernel framing pushes toward treating the asymmetry as an unchangeable premise, while the interpretive-institution framing exposes it as a constructed, contestable administrative choice.',
    'This story adopts the interpretive-institution framing (consistent with the coordination_function and transfer_function answers above, and with the fact that reform has occurred repeatedly within the tradition — 1939 Act, 1986 Act, 2019 Act) because the schema''s guidance for kernel readings treats the reading as an institutional/legal arrangement under contest, not as a claim about divine textual meaning itself.',
    'Had the textual-kernel framing been adopted instead, extractiveness and suppression would likely be authored lower and accessibility_collapse higher, potentially pushing the classification toward mountain from the perspective of adherents who treat the text itself as beyond revision — this is the ε-invariance boundary case the schema instructs authors to resolve by choosing one framing per story rather than blending them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the constraint is best framed as the fixed textual kernel or the contestable interpretive institution administering it; this story adopts the institutional framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__muslim_shariat_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1937, family_law_authority__muslim_shariat_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(fami_tr_t1985, family_law_authority__muslim_shariat_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(fami_tr_t1986, family_law_authority__muslim_shariat_reading, theater_ratio, 1986, 0.2).
narrative_ontology:measurement(fami_tr_t2005, family_law_authority__muslim_shariat_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(fami_tr_t2017, family_law_authority__muslim_shariat_reading, theater_ratio, 2017, 0.26).
narrative_ontology:measurement(fami_tr_t2019, family_law_authority__muslim_shariat_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement(fami_tr_t2024, family_law_authority__muslim_shariat_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fami_be_t1937, family_law_authority__muslim_shariat_reading, base_extractiveness, 1937, 0.62).
narrative_ontology:measurement(fami_be_t1985, family_law_authority__muslim_shariat_reading, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement(fami_be_t1986, family_law_authority__muslim_shariat_reading, base_extractiveness, 1986, 0.64).
narrative_ontology:measurement(fami_be_t2005, family_law_authority__muslim_shariat_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(fami_be_t2017, family_law_authority__muslim_shariat_reading, base_extractiveness, 2017, 0.56).
narrative_ontology:measurement(fami_be_t2019, family_law_authority__muslim_shariat_reading, base_extractiveness, 2019, 0.5).
narrative_ontology:measurement(fami_be_t2024, family_law_authority__muslim_shariat_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1937, family_law_authority__muslim_shariat_reading, suppression_requirement, 1937, 0.4).
narrative_ontology:measurement(fami_su_t1985, family_law_authority__muslim_shariat_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement(fami_su_t1986, family_law_authority__muslim_shariat_reading, suppression_requirement, 1986, 0.55).
narrative_ontology:measurement(fami_su_t2005, family_law_authority__muslim_shariat_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(fami_su_t2017, family_law_authority__muslim_shariat_reading, suppression_requirement, 2017, 0.46).
narrative_ontology:measurement(fami_su_t2019, family_law_authority__muslim_shariat_reading, suppression_requirement, 2019, 0.55).
narrative_ontology:measurement(fami_su_t2024, family_law_authority__muslim_shariat_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__muslim_shariat_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__muslim_shariat_reading, 0.1).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the family_law_authority kernel (the question of what grounds marital authority in a religiously plural constitutional state). Each sibling reading — hindu_dharmashastra_reading, christian_canonical_reading, parsi_zoroastrian_reading, secular_contractual_reading — is authored as its own constraint story with its own ε, beneficiaries, victims, and stakeholder set, per the ε-invariance principle. This reading's ε (0.58) should NOT be compared directly to the siblings' ε values as though measuring 'the same constraint' — each reading has a structurally distinct beneficiary/victim configuration (e.g., the secular_contractual_reading's asymmetries, if any, arise from different mechanisms than talaq/mahr/polygyny). The network edges here mark the shared kernel-contest relationship, not shared ε or shared classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
