% ============================================================================
% CONSTRAINT STORY: family_law_authority__hindu_dharmashastra_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Sacramental Hindu Marriage Regime (Dharmashastra Reading)
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This story instantiates the hindu_dharmashastra_reading of the
 *   family_law_authority kernel: marriage as a sacramental samskara governed
 *   by smritic texts, commentarial tradition, and customary practice, over
 *   the century preceding the 1955-56 Hindu Code codification (interval
 *   points 0-100 map to approximately 1855-1955). The arrangement coordinates
 *   household formation, succession, and ritual continuity while transferring
 *   labor, property, and status from wives, daughters, widows, and lower
 *   castes toward male lineages, officiants, and caste authorities. Epsilon's
 *   referent is the standing dharmic sacramental arrangement as it actually
 *   operated — not the secular-contractual alternative this reading rejects —
 *   with values indexed to what this reading's own framework counts as cost
 *   and benefit. The reading bundles indissolubility, endogamy, coparcenary
 *   exclusion, and the wife's ritual-partner status into one sacramental
 *   package; the tradition itself treats these as inseparable expressions of
 *   a single dharma, so the story authors one stable epsilon for the
 *   integrated arrangement rather than splitting components the reading's own
 *   framework does not separate. KEY AGENTS (by structural relationship): -
 *   male_lineage_heads: primary administrator and principal beneficiary
 *   (powerful/constrained) — controls coparcenary property, arranges
 *   marriages, receives wives' labor and ritual service -
 *   brahmin_priestly_class: secondary beneficiary (organized/identity_locked)
 *   — collects officiant fees; authority and livelihood rest on textual
 *   interpretation - caste_panchayats: enforcement administrators and status
 *   beneficiaries (organized/constrained) — adjudicate endogamy and
 *   propriety, impose ostracism - married_wives: primary target
 *   (powerless/trapped) — bears indissolubility, labor extraction, and
 *   property exclusion - daughters_and_widows: primary target
 *   (powerless/trapped) — excluded from coparcenary; widows under austerity
 *   and remarriage bar - lower_caste_communities: target (powerless/trapped)
 *   — bound by endogamy rules they did not author, disciplined by panchayats
 *   - reform_movements: excluded contestants (organized/mobile) — argue
 *   dissolution, inheritance, and intercaste union through legislatures and
 *   courts - colonial_administration: observer holding the eventual
 *   legislative pen (institutional/analytical) — non-interference eroding
 *   into piecemeal intervention
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, 0.7).
domain_priors:suppression_score(family_law_authority__hindu_dharmashastra_reading, 0.82).
domain_priors:theater_ratio(family_law_authority__hindu_dharmashastra_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hindu_dharmashastra_reading, "Sacramental Hindu Marriage Regime (Dharmashastra Reading)").
narrative_ontology:topic_domain(family_law_authority__hindu_dharmashastra_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hindu_dharmashastra_reading, '4d002a21-916d-4798-94d2-e2cbf4d70c2c').
narrative_ontology:cs_kernel_codification('4d002a21-916d-4798-94d2-e2cbf4d70c2c', fixed_text).
narrative_ontology:cs_authority_grounding('4d002a21-916d-4798-94d2-e2cbf4d70c2c', lineage).
narrative_ontology:cs_interpretation_layer_present('4d002a21-916d-4798-94d2-e2cbf4d70c2c').
narrative_ontology:cs_reading_relation('4d002a21-916d-4798-94d2-e2cbf4d70c2c', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d002a21-916d-4798-94d2-e2cbf4d70c2c', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d002a21-916d-4798-94d2-e2cbf4d70c2c', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d002a21-916d-4798-94d2-e2cbf4d70c2c', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('4d002a21-916d-4798-94d2-e2cbf4d70c2c', foundational, marriage_bond_spans_seven_births).
narrative_ontology:cs_axiom_status(marriage_bond_spans_seven_births, overridden).
narrative_ontology:cs_axiom_grounding('4d002a21-916d-4798-94d2-e2cbf4d70c2c', marriage_bond_spans_seven_births, theological).
narrative_ontology:cs_axiom('4d002a21-916d-4798-94d2-e2cbf4d70c2c', foundational, endogamy_preserves_varna_dharma).
narrative_ontology:cs_axiom_status(endogamy_preserves_varna_dharma, holdable).
narrative_ontology:cs_axiom_grounding('4d002a21-916d-4798-94d2-e2cbf4d70c2c', endogamy_preserves_varna_dharma, deontological).
narrative_ontology:cs_axiom('4d002a21-916d-4798-94d2-e2cbf4d70c2c', secondary, wife_is_sahadharmini_ritual_partner).
narrative_ontology:cs_axiom_status(wife_is_sahadharmini_ritual_partner, holdable).
narrative_ontology:cs_axiom_grounding('4d002a21-916d-4798-94d2-e2cbf4d70c2c', wife_is_sahadharmini_ritual_partner, theological).
narrative_ontology:cs_reference_frame('4d002a21-916d-4798-94d2-e2cbf4d70c2c', dharmic_samskara_lineage_order).
narrative_ontology:cs_drift_state('4d002a21-916d-4798-94d2-e2cbf4d70c2c', eve_of_hindu_code_codification, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('4d002a21-916d-4798-94d2-e2cbf4d70c2c', '').
narrative_ontology:cs_kernel_id(family_law_authority__hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, male_lineage_heads).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, caste_panchayats).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, married_wives).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, daughters_and_widows).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, lower_caste_communities).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, varnashrama_dharma_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, pativrata_wifely_ideal).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, coparcenary_succession_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Head joint households, arrange children's marriages, manage ancestral property as coparcenary holders, and preside over household ritual. Marriage alliances build lineage standing; wives and daughters-in-law supply household labor and ritual service. Stepping outside the dharmic framework would cost them caste standing and ritual legitimacy, so they shape the arrangement from inside it.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, male_lineage_heads, agenda_setter,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__hindu_dharmashastra_reading, male_lineage_heads, beneficiary).

% Officiate the samskara sequence — betrothal, wedding fire rites, and life-cycle ceremonies — collecting dakshina and ceremonial fees from marrying households. Their authority rests on training in Sanskrit textual lineages; interpreting the marriage rules is simultaneously their livelihood, their status, and the identity that constitutes them. Abandoning the vocation would mean losing that identity outright.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, brahmin_priestly_class, beneficiary,
    organized, generational, identity_locked, national).

% Adjudicate marriage disputes, try elopements and intercaste unions, impose fines, ostracism, or expulsion, and certify expiations. Their jurisdiction and prestige depend on communities continuing to bring marriage disputes before them; enforcing endogamy and propriety norms keeps that docket full.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, caste_panchayats, agenda_setter,
    organized, generational, constrained, regional).

% Enter marriage through arrangements made by elders, move into the husband's household, and serve it with labor, obedience, and ritual participation as sahadharmini. The bond admits no dissolution: return to the natal home carries stigma, independent livelihood is scarce, and customary practice bars remarriage for higher-caste widows. Their standing depends on fulfilling the pativrata ideal.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, married_wives, payer,
    powerless, biographical, trapped, national).

% Daughters are raised as gifts to another lineage — excluded from ancestral coparcenary shares, endowed at marriage, and absorbed into the husband's family. Widows of the stricter communities live under austerity observance, dependent on sons or natal kin, barred from remarriage and from inheriting their husbands' shares.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, daughters_and_widows, payer,
    powerless, biographical, trapped, national).

% Are bound by endogamy rules they did not author and sit at the bottom of the hierarchy those rules rank. Village panchayats discipline their marriages and punish transgression; adopting upper-caste marital strictness is the price of status claims, while deviation invites ostracism or worse. Many communities' older customs permitted widow remarriage, but Sanskritizing pressure steadily erodes those openings.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, lower_caste_communities, payer,
    powerless, biographical, trapped, national).

% Widow-remarriage advocates, Brahmo and Arya Samaj reformers, and later the Hindu Code Bill's sponsors argue for dissoluble marriage, daughters' inheritance, and intercaste union. Orthodox pandit assemblies control textual interpretation and refuse them standing, so they argue through colonial legislatures, courts, and print instead of the councils where marriage rules are authoritatively stated.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, reform_movements, excluded,
    organized, biographical, mobile, national).

% Governs under a policy of administering Hindus by Hindu law, translating dharmic texts for its courts and initially declining to touch marriage. Across the interval it is drawn in piecemeal — the Widows' Remarriage Act, the Age of Consent Act, the Special Marriage Act — accumulating the legislative pen that will eventually write the 1955 codification.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, colonial_administration, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__hindu_dharmashastra_reading, male_lineage_heads).
narrative_ontology:fixing_cost_class(family_law_authority__hindu_dharmashastra_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates household formation and lineage continuity: matches spouses across families, sequences the life-cycle rites, devolves ancestral property through the male line, assigns care obligations for elders and dependents, and marks caste membership at each generation — all without centralized state machinery.
% TRANSFER_FUNCTION: Moves productive and reproductive labor, obedience, and ritual service from wives and daughters-in-law into husbands' lineages; moves ancestral property along male lines while excluding daughters and widows from shares; moves dakshina and ceremonial fees from marrying households to priestly officiants; moves deference and status up the caste hierarchy through endogamy.
% ABSENT_VOICES: Women themselves — wives, daughters, widows — had no seat in the pandit assemblies or caste councils where the rules were interpreted and adjudicated; lower castes were subject to norms they did not author; young people resisting arranged matches and intercaste couples had no forum inside the system. They stand outside the shastric councils and panchayats, appearing only as subjects of rulings or, late in the interval, as petitioners to colonial courts and legislatures.
% DISAPPEARANCE_RATIONALE: Households would form and dissolve by choice, property would devolve by will or equal shares, caste boundaries would lose their matrimonial enforcement, the priestly officiant economy would shrink to the voluntarily devout, and panchayat jurisdiction over marriage would lapse — approximately the rearrangement that in fact followed the 1955-56 codification.
% FOUNDING_PROBLEM: Lineage continuity under conditions with no state probate, welfare, or old-age support: securing lawful heirs to perform ancestor rites and inherit, provisioning widows and elders within the household, and transmitting dharma across generations through the samskara sequence.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: colonial judicial records, census and district-gazetteer ethnography, and missionary documentation attest the succession-and-dependency problems the arrangement managed. In the Hindu Code Bill debates, B. R. Ambedkar and reformist witnesses attest that probate courts, wage labor, and emerging state welfare now cover much of that ground, while orthodox pandit associations and caste sabhas attest the problem remains live. Both attestations come from outside the beneficiary set and they disagree — hence contested.
narrative_ontology:disappearance_verdict(family_law_authority__hindu_dharmashastra_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__hindu_dharmashastra_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__hindu_dharmashastra_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__hindu_dharmashastra_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__hindu_dharmashastra_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.70 at interval end) because the arrangement transfers property, labor, and ritual service from wives, daughters, and lower castes to male lineages and officiants, with the transfer enforced rather than exchanged. Suppression (0.82) is structural first — indissolubility, coparcenary exclusion, no remarriage market, household confinement — with an internalized pativrata layer tracked by omega. Theater (0.30) is moderate: the samskara sequence performs real coordination (household formation, succession, elder care), but a growing share of ritual activity by interval end legitimates the property and status order rather than producing coordination goods. Accessibility collapse (0.58): alternatives existed — renunciation, custom-level variance, conversion — but collapsed sharply once the normative order's penalties were understood. Resistance (0.62) rose across the century, from widow-remarriage advocacy through the Age of Consent controversy to the Code Bill fights. The measurement series show an enforcement ratchet: suppression_requirement climbs as orthodox mobilization works harder against widening reform pressure, theater climbs as legitimation displaces function, and extraction creeps up as rising property values enlarge the joint-family stakes. All three series share one six-point grid (0/20/40/60/80/100) so no metric is sampled against another's end-state. Coordination type is identity_coordination: the function whose failure would break the arrangement is membership-boundary maintenance (lineage, caste, ritual continuity); attachment bonds and property flows ride on it. The FNL gaming risk is live here — identity framing ('this is our dharma') is also the arrangement's cover story — so the floor stays at the type default rather than being raised.
 *
 * PERSPECTIVAL GAP:
 *   From the karta's seat the arrangement is the inherited order he administers and profits from — coordination whose costs land elsewhere; from a wife's seat the same structure is confinement without exit; from a panchayat's seat it is jurisdiction and standing; from the priesthood's seat it is livelihood fused with identity. The engine computes these per-seat classifications from the power, exit, and role data; the authored claim does not adjudicate between them, and the divergence between the beneficiary seats' coordination experience and the payer seats' extraction experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place male_lineage_heads, brahmin_priestly_class, and caste_panchayats near the subsidized end (low d); victim declarations place married_wives, daughters_and_widows, and lower_caste_communities near the full-target end (high d), amplified by trapped exit across all three payer seats. No directionality_overrides are authored: the derivation from declarations plus exit options already separates the seats correctly, and because overrides key on power atoms rather than named agents, an override tuned for one organized actor (say the priesthood) would misfire on the panchayats and reform movements sharing that atom. The dual-positioned karta (agenda_setter with beneficiary secondary) derives correctly from his beneficiary declaration; his administrative role does not reduce what he collects.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as pure extraction misses the real coordination it performed for centuries — household formation, succession, elder care, ritual continuity — with no state probate or welfare machinery behind it; reading it as pure coordination misses the enforced asymmetry of who paid. The tangled_rope claim keeps both visible. On the R5 interview the founding problem reads contested: lineage continuity persists as a human problem, but probate law, wage labor, and state welfare now handle much of what the joint family solved, and the gap between a dying founding mandate and a persisting arrangement is exactly what the codification-era measurements register. The mandatrophy lens prevents two errors: dismissing the arrangement's genuine historical coordination function as mere cover, and crediting its late-interval operation — increasingly theatrical legitimation atop hardened enforcement — with a coordination function it was no longer primarily performing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the family_law_authority kernel; what would each sibling reading change structurally if instantiated?',
    'Compile and compare the sibling stories: muslim_shariat_reading (dissolvable nikah contract — different exit structure and party ontology), christian_canonical_reading (ecclesiastical annulment machinery), parsi_zoroastrian_reading (community-council governance of marriage), secular_contractual_reading (autonomous individual contractors under state registry).',
    'Victim sets, exit options, and epsilon shift per reading; this story''s classification must not average across readings — the disagreement is located in marriage''s ontological status (sacrament versus contract), the exit structure (indissolubility versus dissolution machinery), and who counts as a party (lineages and castes versus autonomous individuals).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame routing: one reading of a contested kernel, siblings are separate constraints.').

omega_variable(
    sacramental_naturality_ambiguity,
    'Is the marriage bond''s indissolubility a sacral fact binding across births, or a constructed rule that concentrates property and labor in male lineages?',
    'Cross-cultural comparison of marriage-dissolution regimes under comparable material conditions; test whether indissolubility tracks theological commitment or property concentration patterns.',
    'If constructed-with-beneficiaries, the arrangement''s claimed naturalness is a false summit and the extraction assessment rises; if genuinely sacral to participants, part of the measured cost is constitutive of a good the participants themselves endorse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_naturality_ambiguity, conceptual, 'Natural-law versus constructed-status ambiguity of the indissolubility core.').

omega_variable(
    customary_shastra_divergence,
    'Does textual shastra or local custom govern in practice, given documented divergence (widespread lower-caste widow remarriage, matrilineal inheritance in Malabar)?',
    'District-level ethnographic and court records correlating actual marriage practice with caste, region, and degree of textual exposure across the interval.',
    'Where custom governs, measured extraction falls for those communities; where Sanskritization spreads textual strictness, extraction generalizes — the effective referent of the metric shifts by region and decade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_shastra_divergence, empirical, 'Text-versus-custom governance ambiguity affecting the extraction measure''s reach.').

omega_variable(
    stridhan_buffer_ambiguity,
    'Did women''s separate property (stridhan) constitute a real economic buffer softening the property exclusion, or a nominal category controlled in practice by the husband''s family?',
    'Colonial court records on stridhan disputes and estate accounting; judicial commentary on women''s actual disposal rights versus formal entitlement.',
    'A real buffer lowers effective extraction from wives and daughters; a nominal one confirms the coparcenary exclusion as the core of the property extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stridhan_buffer_ambiguity, empirical, 'Whether the doctrinal women''s-property category had economic substance.').

omega_variable(
    suppression_internalization_split,
    'Is the suppression holding wives in place structural (economic dependency, no remarriage market, household confinement) or internalized (pativrata identity making exit unthinkable)?',
    'Post-exit trajectories: women who left via conversion, refuge institutions, or the post-1856 widow-remarriage opening — does distress and norm-adherence persist after the structural barriers fall?',
    'If heavily internalized, effective suppression exceeds the structural measure and outlives legal reform; the 1955 statutes alone would not dissolve the constraint, and the piton-risk horizon extends past codification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized suppression mechanism in the wife''s position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__hindu_dharmashastra_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement_basis(fami_tr_t0, observed).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(fami_tr_t20, observed).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(fami_tr_t40, observed).
narrative_ontology:measurement(fami_tr_t60, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement_basis(fami_tr_t60, observed).
narrative_ontology:measurement(fami_tr_t80, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement_basis(fami_tr_t80, observed).
narrative_ontology:measurement(fami_tr_t100, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement_basis(fami_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(fami_be_t0, observed).
narrative_ontology:measurement(fami_be_t20, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(fami_be_t20, observed).
narrative_ontology:measurement(fami_be_t40, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement_basis(fami_be_t40, observed).
narrative_ontology:measurement(fami_be_t60, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 60, 0.67).
narrative_ontology:measurement_basis(fami_be_t60, observed).
narrative_ontology:measurement(fami_be_t80, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 80, 0.69).
narrative_ontology:measurement_basis(fami_be_t80, observed).
narrative_ontology:measurement(fami_be_t100, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 100, 0.7).
narrative_ontology:measurement_basis(fami_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(fami_su_t0, observed).
narrative_ontology:measurement(fami_su_t20, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(fami_su_t20, observed).
narrative_ontology:measurement(fami_su_t40, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(fami_su_t40, observed).
narrative_ontology:measurement(fami_su_t60, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement_basis(fami_su_t60, observed).
narrative_ontology:measurement(fami_su_t80, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 80, 0.79).
narrative_ontology:measurement_basis(fami_su_t80, observed).
narrative_ontology:measurement(fami_su_t100, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 100, 0.82).
narrative_ontology:measurement_basis(fami_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__hindu_dharmashastra_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% The family_law_authority kernel decomposes into five readings: this file plus muslim_shariat_reading, christian_canonical_reading, parsi_zoroastrian_reading, and secular_contractual_reading. Each reading is a separate constraint with its own epsilon, beneficiary/victim structure, and exit architecture. They are linked because the dharmic reading's persistence shaped the others' operating environment (personal-law pluralism made the secular reading opt-in rather than default) and because codification-era pressure propagated across all of them. Epsilon differs by reading: the shariat reading's dissolution machinery lowers exit-suppression relative to this reading's indissolubility; the secular reading's autonomous-contractor ontology removes the lineage-as-party structure entirely.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
