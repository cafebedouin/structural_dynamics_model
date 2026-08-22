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
 *   human_readable: Marriage Authority: Muslim Shariat Reading (Nikah as Civil Contract)
 *   domain: religious/legal/political
 *
 * SUMMARY:
 *   This constraint instantiates the Muslim shariat reading of the contested
 *   family-law-authority kernel: marriage (nikah) as a civil contract whose
 *   formation, obligations, and dissolution are governed by Quranic
 *   injunctions and hadith, mediated through Islamic jurisprudence (fiqh) and
 *   the ulema authority structure. The reading permits polygyny, assigns
 *   unilateral divorce (talaq) to husbands, obligates the groom's mahr
 *   (dower) to the bride, and vests guardianship (wilayah) authority in male
 *   relatives. Pre-2019, divorce access was sharply gender-asymmetric; the
 *   2019 Indian triple-talaq ban and similar reforms in Muslim-majority
 *   states represent pressure on the constraint but do not invalidate the
 *   core reading's theoretical structure (which permits male talaq). The
 *   authoring seat is the reading's own theological commitment: what this
 *   constraint IS as the shariat jurist and believer understands it, not what
 *   it appears from the secular-law or gender-rights seats. This is ONE
 *   reading of a contested kernel; the sibling readings (Christian canonical,
 *   Hindu dharmashastra, Parsi Zoroastrian, secular contractual) are OTHER
 *   constraints, not covered here.
 *
 * KEY AGENTS:
 *   - Religious patriarchs (ulema, mufti, qadi): institutional authority; identity-locked to the reading; set rules and interpret precedent
 *   - Male family heads: beneficiary seat; hold talaq and polygyny rights; have arbitrage exit (migrate to secular jurisdictions)
 *   - Women seeking divorce: victim seat; powerless; identity-locked (Muslim community membership fused with accepting the reading); trapped by gender-asymmetric dissolution rules
 *   - Non-consenting marriage parties: victim seat; trapped; guardianship authority overrides consent
 *   - State secular authority: excluded seat; competes with shariat reading for adjudicatory power in Muslim-majority democracies
 *   - Women's-rights advocates: excluded seat; mounted resistance through state reform, international venues, and reformist reinterpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__muslim_shariat_reading, 0.62).
domain_priors:suppression_score(family_law_authority__muslim_shariat_reading, 0.71).
domain_priors:theater_ratio(family_law_authority__muslim_shariat_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(family_law_authority__muslim_shariat_reading, resistance, 0.56).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__muslim_shariat_reading, "Marriage Authority: Muslim Shariat Reading (Nikah as Civil Contract)").
narrative_ontology:topic_domain(family_law_authority__muslim_shariat_reading, "religious/legal/political").

domain_priors:requires_active_enforcement(family_law_authority__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__muslim_shariat_reading, '3d6ba025-b173-4f71-945b-6d2392c906e8').
narrative_ontology:cs_kernel_codification('3d6ba025-b173-4f71-945b-6d2392c906e8', fixed_text).
narrative_ontology:cs_authority_grounding('3d6ba025-b173-4f71-945b-6d2392c906e8', lineage).
narrative_ontology:cs_interpretation_layer_present('3d6ba025-b173-4f71-945b-6d2392c906e8').
narrative_ontology:cs_reading_relation('3d6ba025-b173-4f71-945b-6d2392c906e8', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d6ba025-b173-4f71-945b-6d2392c906e8', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d6ba025-b173-4f71-945b-6d2392c906e8', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d6ba025-b173-4f71-945b-6d2392c906e8', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('3d6ba025-b173-4f71-945b-6d2392c906e8', foundational, quranic_injunction_binding_over_custom).
narrative_ontology:cs_axiom_status(quranic_injunction_binding_over_custom, holdable).
narrative_ontology:cs_axiom_grounding('3d6ba025-b173-4f71-945b-6d2392c906e8', quranic_injunction_binding_over_custom, theological).
narrative_ontology:cs_axiom('3d6ba025-b173-4f71-945b-6d2392c906e8', foundational, male_talaq_right_quranic_mandate).
narrative_ontology:cs_axiom_status(male_talaq_right_quranic_mandate, holdable).
narrative_ontology:cs_axiom_grounding('3d6ba025-b173-4f71-945b-6d2392c906e8', male_talaq_right_quranic_mandate, empirically_contingent).
narrative_ontology:cs_reference_frame('3d6ba025-b173-4f71-945b-6d2392c906e8', quranic_hadith_authority_over_family).
narrative_ontology:cs_drift_state('3d6ba025-b173-4f71-945b-6d2392c906e8', post_2000_nation_state_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3d6ba025-b173-4f71-945b-6d2392c906e8', '').
narrative_ontology:cs_kernel_id(family_law_authority__muslim_shariat_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, religious_patriarchs).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, male_family_heads).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, ulema_jurists).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, women_seeking_divorce).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, non_consenting_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__muslim_shariat_reading, families_and_communities).
narrative_ontology:constraint_victim(family_law_authority__muslim_shariat_reading, families_and_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious scholars and institutional authorities (mufti, qadi, mosque leadership) who interpret and transmit Quranic and hadith rules governing marriage, divorce, and family relations. They adjudicate disputes, validate marriages, and maintain the interpretive tradition. Their position depends on the standing authority of these sources; challenging the reading would dissolve their institutional standing.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, religious_patriarchs, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Men who hold talaq (unilateral divorce) right under classical shariat interpretation; retain polygynous marriage rights; receive mahr obligation from bride's family; retain guardianship authority (wilayah) in marriage and property matters. They can exit this arrangement by migrating to secular legal jurisdictions, adopting alternative religious readings, or accepting state law constraints (as occurs increasingly in Muslim-majority democracies post-2000).
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, male_family_heads, beneficiary,
    powerful, generational, arbitrage, regional).

% Women bound by classical shariat rules requiring male consent or judicial intervention to dissolve marriage; must prove grounds (harm, abandonment, impotence) before a qadi; cannot exercise unilateral talaq. Their identity as Muslim, daughter, mother, and community member fuses with accepting family law constraints; exit requires rejecting religious framework entirely or migrating. Pre-2019, no legal remedy short of decades of litigation or personal catastrophe.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, women_seeking_divorce, payer,
    powerless, biographical, identity_locked, regional).

% Parties to marriage arrangements (particularly women in forced/early marriages, children contracted by guardians) who do not consent to the contract but lack legal standing to object under classical shariat interpretation. Guardianship authority (wali) permits marriage on behalf of even adult women in some interpretations. No exit until the marriage dissolves by death, divorce (if male consents), or judicial intervention (rare and difficult).
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, non_consenting_parties, payer,
    powerless, biographical, trapped, regional).

% The scholarly class whose interpretation and transmission of Islamic jurisprudence (fiqh) grounds the authority structure. They derive family law from Quran, hadith, ijma (consensus), and qiyas (analogy). Their professional and spiritual identity rests on the standing authority of these sources; reinterpreting family law in line with gender equality would require either reformist ijtihad (reinterpretation) or conceding state law authority — both threaten traditional authority structures.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, ulema_jurists, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, ulema_jurists, beneficiary).

% Civil law authorities in Muslim-majority nation-states that have reformed or superseded classical shariat family law (Egypt 2000, Tunisia, Morocco 2004, India personal law courts). These authorities are excluded from the shariat reading's interpretive framework but compete with it for adjudicatory authority; state reforms override shariat rules, creating structural conflict.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, state_secular_authority, excluded,
    institutional, generational, arbitrage, national).

% Feminist and gender-justice movements (both Muslim and secular) that contest gender-asymmetric divorce, polygyny, forced marriage, and guardianship rules. They are excluded from the classical shariat framework's legitimacy structure but mount resistance through state legal reform, international human rights venues, and interpretive reinterpretation (modern Islamic feminism). Their analysis is not admitted as authoritative within the traditional ulema-centered reading.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, women_rights_advocates, excluded,
    organized, generational, mobile, global).

% Extended family structures and communities that benefit from stable marriage institutions (social cohesion, property transmission, legitimacy of children) but also bear costs where individuals are trapped in harmful marriages, where gender asymmetry creates conflict, or where state law intrudes on community authority. Their stake is in family stability; they experience the constraint's extraction when that stability requires enduring harm.
narrative_ontology:constraint_stakeholder(family_law_authority__muslim_shariat_reading, families_and_communities, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(family_law_authority__muslim_shariat_reading, families_and_communities, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__muslim_shariat_reading, religious_patriarchs).
narrative_ontology:fixing_cost_class(family_law_authority__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, religiously legitimated marriage contract framework that governs property transfer (mahr), sexual access rights, guardianship, inheritance, and dissolution across Muslim communities. Solves the coordination problem of what makes a marriage valid, what rights and obligations attach to it, and how it may be dissolved — uses Quranic and prophetic precedent rather than state secular law.
% TRANSFER_FUNCTION: Transfers authority over marriage formation, dissolution, and family relations from state law to religious jurisprudence; transfers divorce-initiation power asymmetrically from women to men (talaq right); transfers bride-price obligation (mahr) from bridegroom's to bride's family; transfers guardianship authority (wilayah) to male relatives.
% ABSENT_VOICES: Women who reject gender-asymmetric divorce rules; secular-law advocates who dispute religious law authority; reformist Muslims proposing ijtihad-based reinterpretation; minority wives in polygynous marriages who are not consulted on subsequent marriages; children and forced-marriage victims who have no voice in guardianship-based contracting. These voices are structurally excluded from the classical ulema-centered interpretive framework.
% DISAPPEARANCE_RATIONALE: If the shariat family law reading and its enforcement vanished overnight, Muslim communities would reorganize around state civil law (as has occurred in Egypt, Tunisia, Morocco, Indonesia post-2000). Marriage would be contracted under civil authority; divorce would require mutual consent or judicial grounds applied symmetrically; guardianship would face state family court oversight; property rights would follow civil code. The institutional authority of the ulema-centered reading would collapse; communities would lose a religiously legitimated framework but gain gender-symmetric legal remedies.
% FOUNDING_PROBLEM: Early Islamic societies needed a framework for marriage, property relations, and social stability that grounded authority in revelation and prophetic precedent rather than pre-Islamic custom (jahiliyya). The Quranic injunctions and hadith were interpreted to regulate sexual access, ensure rights for spouses and children, and establish property obligations (mahr), replacing Arab tribal custom with religiously legitimated rules.
% FOUNDING_PROBLEM_CORROBORATION: Classical Islamic historians attest the founding problem was live (replacement of jahiliyya custom). Contemporary reformist scholars and women's-rights organizations attest the founding problem is substantially solved: a stable marriage framework exists, children's legitimacy is established, and property rights are secured — but argue the gender-asymmetric features (talaq, polygyny, guardianship rules) are cultural accretions, not core solutions. State legislatures in Muslim-majority countries have voted to modify or supersede these rules (Egypt 2000, Morocco 2004, Tunisia), attesting that alternative solutions are workable.
narrative_ontology:disappearance_verdict(family_law_authority__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__muslim_shariat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__muslim_shariat_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The constraint is CLAIMED as tangled_rope (genuine coordination function + asymmetric extraction) because the reading establishes real institutional order (marriage validity, property rights, family stability) while simultaneously concentrating divorce, polygyny, and guardianship power in male seats and excluding women from full contractual autonomy. Extractiveness runs 0.48→0.62 over the interval, rising modestly as state-legal alternatives and women's-rights pressure force the ulema to defend the reading more actively and defensively. Suppression is high (0.64→0.71) because enforcement requires active exclusion: state law must be kept out, reformist interpretations must be delegitimized, women's voices must be kept from authoritative tables. Theater rises from 0.12→0.28 as the constraint ages (post-2000): increasingly, the rhetorical and defensive work of justifying gender asymmetry becomes more prominent relative to the original coordination function. The measurements run on one shared grid; all three metrics are authored at each time point. The slope flattens after t=15 (year 2015 reference), reflecting saturation: extractiveness stops rising because state-law competition has already restructured the constraint in many jurisdictions; suppression plateaus because the ulema has stabilized its defensive posture; theater plateaus because the explanatory burden reaches a steady state. The constraint is NOT claimed as snare because genuine coordination function persists (marriage validity, property regime, family structure); if the coordination function had atrophied entirely, the type would shift to piton.
 *
 * PERSPECTIVAL GAP:
 *   The male-family-heads seat and the women-seeking-divorce seat compute radically different types from the same structural data. The male beneficiary seat perceives the constraint as rope (genuine coordination, fair allocation of obligations, legitimate authority). The female victim seat perceives it as snare (the coordination story masks extraction, consent is nominal, exit is impossible). The agenda-setter seat (ulema) experiences it as a pure coordination mechanism that happens to allocate authority asymmetrically because that is what the sources command. The engine computes each seat's effective extraction (χ) from directionality (d), beneficiary/victim status, exit options, and scope; the divergent computations reveal the structural asymmetry that the single 'claimed_type' claim cannot capture alone. The claim/metric independence rule means we state the reading's own theological understanding (tangled_rope: real coordination + asymmetric extraction acknowledged within the framework) rather than tuning the claim toward a predicted computation.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious patriarchs: d approaches 0.0 (beneficiary); they collect authority, maintain interpretive monopoly, face no exit pressure. Male family heads: d ranges 0.15-0.35 (slight beneficiary); they hold talaq and polygyny rights but also bear obligations (mahr, maintenance); their arbitrage exit is real (migrate to secular law, adopt reformist readings). Women seeking divorce: d approaches 1.0 (full target); they bear gender-asymmetric extraction with identity-locked exit (Muslim identity fused with accepting the framework, community sanctions prevent exit). Non-consenting parties: d = 1.0 (trapped); no exit, no choice, pure extraction. State secular authority sits as excluded (d = 0.5 by default; neither benefits nor pays within the shariat reading, but competes structurally). Women's advocates sit as excluded/analytical (d = 0.5; they are organized enough to mount resistance but lack standing within the reading's legitimacy structure). The derivation from beneficiary (religious patriarchs, male family heads) and victim (women, non-consenting parties) declarations, combined with exit-option modulation, produces these d values. No overrides are needed; the structural data is unambiguous.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (replacement of jahiliyya custom with stable, religiously legitimated family framework) was live for the first 1,200 years. The problem becomes contested around 1850 with colonial state intrusion; sharply contested after 1950 with nation-state legal systems and women's education; substantially dead by 2000 in Muslim-majority democracies, which have reformulated or superseded shariat family law (Egypt 2000, Tunisia, Morocco 2004, Indonesia). Yet the constraint persists in many contexts despite the founding problem's death. This is the mandatrophy signature: the arrangement that solved a real coordination problem now persists as pure rent collection by the beneficiary seats (ulema authority, male divorce power) because the coordinate function has been adequately supplied by state law alternatives while the extraction benefits remain concentrated enough to defend. The theater-ratio rise (0.12→0.28) documents the increasing proportion of activity devoted to defending the constraint's legitimacy rather than performing its coordination function. The constraint does NOT dissolve because: (1) exit costs remain high for women (religious identity, community sanctions, social isolation); (2) the ulema retains institutional power in many jurisdictions; (3) reformist challenge (like ijtihad-based equality readings) has not achieved consensus enough to dislodge the classical reading as the default authority structure. Mandatrophy is resolved in favor of persistence by suppression and identity-locking, not by genuine coordination need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gender_asymmetry_foundational_or_cultural,
    'Are the gender-asymmetric features of classical shariat family law (talaq, polygyny, guardianship) mandated by Quranic injunction and hadith (foundational to the reading), or are they cultural accretions from pre-Islamic and early-Islamic custom (contingent to the reading)?',
    'Comparative Quranic exegesis and hadith-criticism: do the sources REQUIRE talaq asymmetry and polygyny, or do they PERMIT them? Can a reading be constructed that permits only mutual-consent divorce and bars polygyny while remaining faithful to the sources? Reformist Islamic jurisprudence (ijtihad) addresses this directly; consensus among contemporary exegetes would resolve it.',
    'If gender asymmetry is foundational, the reading is structurally committed to extracting from women-as-victims. If cultural, a reformist reinterpretation could strip the extraction while preserving the coordination function and the reading''s standing as authentically Islamic. This determines whether the constraint is inherently a tangled_rope or whether it could be reformed into a rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gender_asymmetry_foundational_or_cultural, conceptual, 'Whether gender asymmetry is mandated by sources or culturally contingent.').

omega_variable(
    suppression_mechanism_structural_or_internalized,
    'Is the measured suppression of women''s divorce access enforced structurally (judicial barriers, guardianship authority, community sanctions) or internalized (women believe they deserve the restrictions, have internalized subordination, or fused identity with accepting the framework)?',
    'Post-exit observation: in jurisdictions where shariat family law has been formally superseded (Egypt 2000, Tunisia, Morocco 2004), do women who were previously trapped by classical shariat continue to exhibit suppression behaviors (reluctance to remarry, acceptance of inequality, deference to male authority) after legal remedies are available? If internalization is substantial, suppression trajectories will remain elevated even after structural barriers are removed.',
    'If suppression is mostly structural, removing state enforcement of shariat rules (as occurred in Egypt 2000) should rapidly reduce suppression and enable exit. If suppression is substantially internalized, legal reform will be necessary but insufficient; women will require additional support (consciousness-raising, education, economic independence) to exercise new legal rights. This distinction determines the cost and feasibility of remedying the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_or_internalized, empirical, 'Whether suppression of women''s divorce access is structural barriers or internalized belief/identity.').

omega_variable(
    ulema_identity_lock_mechanism,
    'What specifically locks the ulema''s identity to defending the classical shariat reading? Is it theological commitment (sincere belief in the reading''s correctness), professional interest (institutional authority depends on maintaining the reading), or some fusion of both?',
    'Biographical and institutional analysis: do reformist scholars who challenge gender-asymmetric rules face professional sanctions (loss of position, delegitimization, exclusion from scholarly networks)? Do scholars express genuine theological disagreement or institutional pressure? Do scholars who emigrate to secular jurisdictions continue to defend the classical reading, or do they shift to reformist positions once professional incentives change?',
    'If the lock is purely theological, reformist reinterpretation would require theological breakthrough from within the ulema. If the lock is substantially institutional/professional, removing institutional stakes (e.g., by empowering secular legal authority) could allow the ulema to reinterpret. This determines whether constraint reform requires theological transformation or institutional restructuring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ulema_identity_lock_mechanism, empirical, 'Whether ulema identity-locking is theological conviction or institutional interest.').

omega_variable(
    kernel_contest_reading_exclusivity,
    'Within a single Muslim community or family, can the shariat reading coexist with an adopted secular-legal reading (two parties to the same marriage contract invoking different authority systems), or do they logically foreclose each other?',
    'Institutional analysis of Muslim-majority democracies post-2000: do individuals and families adopt dual-track compliance (religious ceremony under shariat understanding + civil registration under state law), or do they choose exclusive authority? Can a marriage be ''Islamic'' (per the shariat reading) and ''legally registered'' (per state law) simultaneously, or is the state law adoption a rejection of the shariat reading?',
    'If readings coexist, the kernel is genuinely contested and multiple readings can persist in the same jurisdiction. If readings foreclose each other, adoption of state law authority constitutes a rejection of the shariat reading, and constraint reform becomes a zero-sum game. This determines whether the constraint can evolve through coexistence or only through replacement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_reading_exclusivity, empirical, 'Whether the Muslim shariat reading and secular-contractual reading coexist or foreclose each other in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__muslim_shariat_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__muslim_shariat_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fami_tr_t5, family_law_authority__muslim_shariat_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__muslim_shariat_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(fami_tr_t15, family_law_authority__muslim_shariat_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__muslim_shariat_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(fami_tr_t25, family_law_authority__muslim_shariat_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__muslim_shariat_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(fami_be_t5, family_law_authority__muslim_shariat_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(fami_be_t10, family_law_authority__muslim_shariat_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(fami_be_t15, family_law_authority__muslim_shariat_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(fami_be_t20, family_law_authority__muslim_shariat_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(fami_be_t25, family_law_authority__muslim_shariat_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__muslim_shariat_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(fami_su_t5, family_law_authority__muslim_shariat_reading, suppression_requirement, 5, 0.67).
narrative_ontology:measurement(fami_su_t10, family_law_authority__muslim_shariat_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(fami_su_t15, family_law_authority__muslim_shariat_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(fami_su_t20, family_law_authority__muslim_shariat_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(fami_su_t25, family_law_authority__muslim_shariat_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__muslim_shariat_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(family_law_authority__muslim_shariat_reading, 0.12).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__muslim_shariat_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested family-law-authority kernel. The Muslim shariat reading grounds family law in Quranic injunction and hadith; it is ONE of five sibling readings of the same kernel. Each reading instantiates a different constraint with a different ε value, beneficiary structure, and victim set. The readings coexist in different jurisdictions and within pluralistic societies as competing authority claims. Network edges link all readings of the same kernel; each reading's `network.affects_constraints` array names the siblings it structurally influences or is foreclosed by. The decomposition respects ε-invariance: within this reading, ε is assessed by the reading's own lights (the standing shariat arrangement as the reading understands it); other readings assess ε differently from their own commitments and may produce different measurements (e.g., the secular reading sees gender-symmetric extraction; this reading sees gender-asymmetric extraction defended as proportional to obligations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
