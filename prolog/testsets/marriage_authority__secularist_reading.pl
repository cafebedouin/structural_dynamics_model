% ============================================================================
% CONSTRAINT STORY: marriage_authority__secularist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__secularist_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority__secularist_reading
 *   human_readable: Marriage Authority: Secular Modernist Reading (UCC Elimination of Personal Law)
 *   domain: legal_pluralism/constitutional_law/family_law
 *
 * SUMMARY:
 *   This constraint instantiates the secularist reading of the marriage
 *   authority kernel—the claim that personal law pluralism is a transitional
 *   anomaly and democratic legislatures should enact a Uniform Civil Code
 *   (UCC) eliminating religious family law governance. The reading frames
 *   secular, unified family law as the only legitimate endpoint of
 *   constitutional modernization. The structural beneficiary is the secular
 *   modernist coalition and the democratic legislature itself; the structural
 *   victims are minority religious communities whose family law authority
 *   would be eliminated. Suppression is high (0.71) because the elimination
 *   requires active enforcement: state courts must reject personal law
 *   claims, personal law practitioners must be displaced, and religious
 *   community authority must be systematically delegitimized. Theater has
 *   been rising (0.18 at 1950 to 0.42 at 2025) as the constraint increasingly
 *   frames itself as inevitable progress rather than contested power
 *   struggle. This reading is in direct conflict with the communal autonomy
 *   reading (which sees pluralism as constitutional protection) and affects
 *   the gender rights and federalist readings by reframing them through the
 *   lens of secular uniformity.
 *
 * KEY AGENTS:
 *   - Secular modernist coalition: judges, legal scholars, reformers who frame UCC as constitutional necessity and personal law as regressive relic
 *   - Democratic legislature: enacts UCC and consolidates state authority over family law
 *   - Minority religious communities: lose legal authority to govern family matters according to tradition; identity-locked victims
 *   - Personal law practitioners: lose professional authority as expertise becomes legally obsolete
 *   - Religious institutional authorities: lose formal authority to adjudicate family disputes; identity-locked victims
 *   - State enforcement apparatus: shifts from pluralist administration to unified enforcement; consolidates state capacity
 *   - Women's rights advocates: beneficiaries via gender-equality framing, but structurally ambiguous—may also experience UCC as majoritarian imposition if drafted without minority women's participation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__secularist_reading, 0.78).
domain_priors:suppression_score(marriage_authority__secularist_reading, 0.71).
domain_priors:theater_ratio(marriage_authority__secularist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Marriage Authority: Secular Modernist Reading (UCC Elimination of Personal Law)").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "legal_pluralism/constitutional_law/family_law").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, '5e1cff7b-bacb-41f0-8c5a-14f6745f3b03').
narrative_ontology:cs_kernel_codification('5e1cff7b-bacb-41f0-8c5a-14f6745f3b03', formalized).
narrative_ontology:cs_authority_grounding('5e1cff7b-bacb-41f0-8c5a-14f6745f3b03', extraction).
narrative_ontology:cs_interpretation_layer_present('5e1cff7b-bacb-41f0-8c5a-14f6745f3b03').
narrative_ontology:cs_reading_relation('5e1cff7b-bacb-41f0-8c5a-14f6745f3b03', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('5e1cff7b-bacb-41f0-8c5a-14f6745f3b03', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('5e1cff7b-bacb-41f0-8c5a-14f6745f3b03', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('5e1cff7b-bacb-41f0-8c5a-14f6745f3b03', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('5e1cff7b-bacb-41f0-8c5a-14f6745f3b03', foundational, secular_authority_constitutionally_required).
narrative_ontology:cs_axiom_status(secular_authority_constitutionally_required, holdable).
narrative_ontology:cs_axiom_grounding('5e1cff7b-bacb-41f0-8c5a-14f6745f3b03', secular_authority_constitutionally_required, deontological).
narrative_ontology:cs_axiom('5e1cff7b-bacb-41f0-8c5a-14f6745f3b03', foundational, legal_pluralism_is_transitory_deviation).
narrative_ontology:cs_axiom_status(legal_pluralism_is_transitory_deviation, holdable).
narrative_ontology:cs_axiom_grounding('5e1cff7b-bacb-41f0-8c5a-14f6745f3b03', legal_pluralism_is_transitory_deviation, empirically_contingent).
narrative_ontology:cs_reference_frame('5e1cff7b-bacb-41f0-8c5a-14f6745f3b03', secular_legislative_supremacy).
narrative_ontology:cs_drift_state('5e1cff7b-bacb-41f0-8c5a-14f6745f3b03', contemporary_ucc_resistance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5e1cff7b-bacb-41f0-8c5a-14f6745f3b03', '').
narrative_ontology:cs_kernel_id(marriage_authority__secularist_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, democratic_legislature).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_religious_communities).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, personal_law_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, women_rights_advocates).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, religious_institutional_authorities).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, state_secular_supremacy).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, unified_citizen_status).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, democratic_majoritarian_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Comprises judges, legal scholars, legislative reformers, and secular intellectuals who believe family law should be uniform, secular, and grounded in democratic legislation rather than religious tradition. They argue personal law pluralism is a vestigial colonial artifact that perpetuates patriarchy, caste discrimination, and communal fragmentation. They benefit from UCC implementation through vindication of their constitutional vision and through institutional consolidation of secular authority over family matters.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, secular_modernist_coalition, beneficiary,
    institutional, generational, analytical, national).

% The elected body tasked with authoring civil law. Under this reading, the legislature is the legitimate arbiter of marriage authority; personal law pluralism represents a failure to exercise this authority. The legislature enacts UCC provisions and enforces them via the state apparatus, consolidating authority over family law that previously fragmented across religious communities.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, democratic_legislature, agenda_setter,
    institutional, generational, analytical, national).

% Muslim, Christian, Jewish, Parsi, and other minority communities whose family law has been governed by religious personal laws. Under UCC, they face the loss of their traditional marriage authority, dispute resolution through religious leaders, and customary practices around inheritance, divorce, and guardianship. Exit from their religious identity framework is not available to them; they either assimilate to secular family law or accept subordination within the new regime.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_religious_communities, payer,
    organized, generational, identity_locked, national).

% Judges, advocates, and scholars trained in personal law jurisprudence who have built careers and institutional authority around religious family law. Under UCC elimination, their expertise becomes legally obsolete; their professional authority is displaced by secular law specialists. They are excluded from the legislative process that determines UCC content, though the constraint directly degrades their standing.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, personal_law_practitioners, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__secularist_reading, personal_law_practitioners, excluded).

% Police, courts, and registrars tasked with enforcing marriage law. Under this reading, state enforcement is shifting from administering multiple personal law codes (a complex but pluralistic system) to enforcing a single UCC. This consolidates state capacity to govern family relations directly rather than mediating religious authority.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Advocates for gender equality within families. Many support UCC on grounds that unified law can mandate equal inheritance, guardianship, and divorce rights across all communities. However, this seat experiences structural ambiguity: some women's movements have disagreed with UCC on grounds that minority women's voices were excluded from its drafting, and that UCC imposes majoritarian Hindu law norms on minorities. This entry captures the beneficiary reading only.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, women_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Intellectuals and community leaders who argue that religious communities have a right to govern family matters according to their traditions. They contend that personal law pluralism is not a vestigial anomaly but a constitutional recognition of group autonomy and protection against majoritarian dominance. They are excluded from the framing of this constraint, though their opposition is substantial.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, communal_autonomy_advocates, excluded,
    organized, generational, mobile, national).

% Heads of religious institutions (muftis, bishops, rabbis, etc.) who derive institutional authority from governing family law within their communities. UCC eliminates their legal authority to adjudicate marriage disputes, inheritance, and guardianship. Their exit is blocked by the civilizational commitment embedded in their role; they cannot abandon their religious mandate.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, religious_institutional_authorities, payer,
    powerful, civilizational, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:fixing_cost_class(marriage_authority__secularist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, legally predictable regime for marriage validity, rights, and dissolution across all citizens regardless of religion. Replaces patchwork administration of multiple personal law codes with a single secular framework that treats all citizens identically.
% TRANSFER_FUNCTION: Moves authority over family relations from religious communities and personal law practitioners to the democratic legislature and secular state apparatus. Transfers interpretive power from religious texts and customary practice to legislative enactment and secular constitutional principles. Transfers the ability to govern one's marriage according to religious tradition to compliance with unified state law.
% ABSENT_VOICES: Minority religious communities and communal autonomy advocates are structurally excluded from the reading that frames their subordination as a transitional anomaly. Intra-community gender-equality advocates are partly present but often subordinated to the UCC framing. Indigenous secular feminism from minority communities is absent from the primary seats named here.
% DISAPPEARANCE_RATIONALE: If the secularist reading's institutional push for UCC disappeared and personal law pluralism was entrenched, family law authority would consolidate around religious communities and customary practice, state family courts would diminish in scope, and the project of uniform secular citizenship would face a constitutive barrier. The institutional and intellectual infrastructure supporting secular legal authority over family matters would revert.
% FOUNDING_PROBLEM: Personal law pluralism inherited from colonialism created administrative fragmentation, perpetuated communal hierarchies and patriarchal practices, and prevented the establishment of a unified modern secular state grounded in equal citizenship. The founding problem is the claim that democratic governance requires secular, unified family law.
% FOUNDING_PROBLEM_CORROBORATION: The secularist coalition and democratic legislatures (and many judges) attest the founding problem is live and UCC is the solution. Minority communities and federalist scholars attest the founding problem is itself misdefined—pluralism is not an anomaly but a feature of constitutional design against majoritarian tyranny. Independent international human-rights bodies have documented human-rights concerns in both directions: gender-equality gaps in personal law AND discriminatory application of UCC impositions on minority communities.
narrative_ontology:disappearance_verdict(marriage_authority__secularist_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__secularist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__secularist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__secularist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__secularist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__secularist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__secularist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint's high extractiveness (0.78 at interval end) reflects that UCC eliminates personal law variation entirely—a zero-sum transfer of authority from communities to state. Suppression (0.71) is substantial because the transfer requires systematic legal displacement of alternative frameworks: personal law courts are marginalized, religious community authority is invalidated, and minority objections are framed as obstacles to progress. Theater ratio has risen from 0.18 to 0.42 because the secularist framing increasingly emphasizes inevitability ('transition to modernity') while the actual mechanism is active legal displacement. Accessibility collapse (0.63) reflects that personal law alternatives are formally available to minorities through cultural practice, but once UCC is legislated, their legal status is erased—alternatives collapse when measured legally. Resistance (0.72) is high because minority communities have mounted substantial constitutional and legislative opposition to UCC, and communal autonomy advocates dispute the entire framing. The measurement series show steady extraction accumulation (rising from 0.35 to 0.78 over 75 years) and suppression intensification (0.42 to 0.71)—the secularist reading's institutional push has accelerated since the 1970s. The claim/metric independence: the story claims tangled_rope (genuine coordination function of legal certainty + extraction) while the metrics describe high extraction with rising theater—the divergence is the measurement point, not a contradiction.
 *
 * PERSPECTIVAL GAP:
 *   From the secular modernist seat, UCC is perceived as solving a genuine coordination problem—harmonizing family law across citizens and eliminating fragmentation. From the minority community seat, the same institutional structure is perceived as majoritarian imposition—the elimination of legal space for their autonomy. The legislative seat perceives authority consolidation as legitimate exercise of sovereign power; the displaced religious community seat perceives it as theft of their traditional authority. These are not different measurements of the same phenomenon—they are genuinely different causal stories with different beneficiaries and victims depending on which seat you occupy. The engine will compute this.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular modernist coalition benefits from UCC enactment and its vindication of their constitutional vision—they are placed as beneficiaries with directionality near the beneficiary end. They hold institutional and analytical power and can exit if they choose (analytical exit_options). Minority religious communities are the clear victims: they lose legal authority over family matters according to their tradition, cannot exit their religious identity (identity_locked exit), and face suppression of alternatives. Their directionality should be near 1.0 (full target). Personal law practitioners are caught as victims despite moderate power—their professional authority is displaced and their exit options are constrained (constrained exit, biographical time horizon). The legislature occupies the role of agenda_setter and beneficiary: it consolidates authority and vindicates the claim that democratic legislatures should govern family law. No directionality override is needed; the structural derivation should produce the intended divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT appear to be mandatrophy—the founding problem (that personal law pluralism creates fragmentation and perpetuates patriarchy) is still asserted as live by the secularist coalition, though minority communities and federalist scholars contest it. The constraint is not an atrophied function maintained theatrically; it is actively enforced institutional project. However, rising theater ratio (0.18 to 0.42) is worth monitoring. If theater ratio reaches 0.55+, it would suggest the constraint is increasingly performing legal inevitability rather than functioning as coordination, signaling possible drift toward piton status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secular_vs_religious_kernel_contest,
    'Is the secularist reading''s core premise—that marriage authority inherently belongs to democratic legislatures, not religious communities—a logically necessary claim of modern constitutionalism, or is it a contestable choice among multiple constitutionally defensible frameworks?',
    'Comparative constitutional analysis of democracies that recognize group autonomy in family law (Canada''s multiculturalism jurisprudence, EU Article 12 jurisprudence on religious family law, federalist models). If other democracies sustain legitimate authority through pluralism rather than secular uniformity, the premise is choice not necessity.',
    'If logically necessary: the reading''s framing of personal law pluralism as ''transitional anomaly'' is grounded in constitutional structure. If choice: the reading becomes a zero-sum power struggle between secular and religious authority, and the beneficiary/victim framing is accurate but not inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_vs_religious_kernel_contest, conceptual, 'Whether secular authority is constitutionally required or one defensible option among others.').

omega_variable(
    minority_vs_equality_tradeoff,
    'Does UCC elimination of personal law codes actually advance gender equality within minority communities, or does it impose majoritarian Hindu law norms on minorities while claiming equality as the justification?',
    'Post-UCC empirical measurement: comparative gender-equality metrics within minority communities before and after UCC implementation; interviews with minority women about whether they experienced UCC as liberation or majoritarian imposition; documentation of whether gender-reform agendas were set by minority women themselves or imposed externally.',
    'If UCC genuinely advances gender equality for minority women: the constraint''s beneficiary framing holds and extraction is justified by coordination. If UCC imposes majoritarian law while displacing minority women''s own reform agendas: extraction is unambiguous and the constraint is snare-disguised-as-tangled-rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_vs_equality_tradeoff, empirical, 'Whether gender equality and minority subordination are separable or inseparable in UCC implementation.').

omega_variable(
    reading_vs_communal_foreclosure,
    'Does the secularist reading''s core claim—that personal law pluralism is a ''transitional anomaly awaiting elimination''—logically foreclose the communal autonomy reading, or can both coexist in competing institutional visions?',
    'Examine whether the two readings could operate in the same constitutional framework where one faction legitimately adopted pluralism and another adopted secular uniformity. If the secularist reading requires its victory to be total (UCC eliminates alternatives), then it forecloses. If both readings could persist in different jurisdictions or institutional domains, they coexist.',
    'If secularist forecloses communal: the constraint is structurally zero-sum and the reading must be coded as foreclosure relation. If both coexist: the readings are competing visions but neither logically eliminates the other, and the relation is coexists_with.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_communal_foreclosure, conceptual, 'Logical relationship between secularist and communal readings of the marriage authority kernel.').

omega_variable(
    internalized_vs_structural_suppression,
    'When minority religious communities accept secular family law, is the acceptance structural suppression (legal bars to alternatives, lack of practical exit routes) or internalized suppression (the community has come to believe secular law is legitimate)?',
    'Post-implementation ethnography: track whether suppression persists after the legal barrier is formally equal (do minority women exit arranged marriages at different rates post-UCC? do community leaders continue to maintain parallel informal dispute resolution despite UCC?). High post-implementation suppression indicates internalized component.',
    'If primarily structural: the constraint''s suppression metric (0.71) captures legal barriers; removal of barriers would reduce suppression. If internalized: the constraint carries its suppression beyond the legal mechanism, and true exit costs are higher than the legal surface suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized mechanisms of suppression under UCC.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1950, marriage_authority__secularist_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement_basis(marr_tr_t1950, projected).
narrative_ontology:measurement(marr_tr_t1970, marriage_authority__secularist_reading, theater_ratio, 1970, 0.24).
narrative_ontology:measurement_basis(marr_tr_t1970, observed).
narrative_ontology:measurement(marr_tr_t1990, marriage_authority__secularist_reading, theater_ratio, 1990, 0.32).
narrative_ontology:measurement_basis(marr_tr_t1990, observed).
narrative_ontology:measurement(marr_tr_t2005, marriage_authority__secularist_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement_basis(marr_tr_t2005, observed).
narrative_ontology:measurement(marr_tr_t2015, marriage_authority__secularist_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement_basis(marr_tr_t2015, observed).
narrative_ontology:measurement(marr_tr_t2025, marriage_authority__secularist_reading, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(marr_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1950, marriage_authority__secularist_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement_basis(marr_be_t1950, projected).
narrative_ontology:measurement(marr_be_t1970, marriage_authority__secularist_reading, base_extractiveness, 1970, 0.48).
narrative_ontology:measurement_basis(marr_be_t1970, observed).
narrative_ontology:measurement(marr_be_t1990, marriage_authority__secularist_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement_basis(marr_be_t1990, observed).
narrative_ontology:measurement(marr_be_t2005, marriage_authority__secularist_reading, base_extractiveness, 2005, 0.72).
narrative_ontology:measurement_basis(marr_be_t2005, observed).
narrative_ontology:measurement(marr_be_t2015, marriage_authority__secularist_reading, base_extractiveness, 2015, 0.76).
narrative_ontology:measurement_basis(marr_be_t2015, observed).
narrative_ontology:measurement(marr_be_t2025, marriage_authority__secularist_reading, base_extractiveness, 2025, 0.78).
narrative_ontology:measurement_basis(marr_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1950, marriage_authority__secularist_reading, suppression_requirement, 1950, 0.42).
narrative_ontology:measurement_basis(marr_su_t1950, projected).
narrative_ontology:measurement(marr_su_t1970, marriage_authority__secularist_reading, suppression_requirement, 1970, 0.54).
narrative_ontology:measurement_basis(marr_su_t1970, observed).
narrative_ontology:measurement(marr_su_t1990, marriage_authority__secularist_reading, suppression_requirement, 1990, 0.63).
narrative_ontology:measurement_basis(marr_su_t1990, observed).
narrative_ontology:measurement(marr_su_t2005, marriage_authority__secularist_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement_basis(marr_su_t2005, observed).
narrative_ontology:measurement(marr_su_t2015, marriage_authority__secularist_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement_basis(marr_su_t2015, observed).
narrative_ontology:measurement(marr_su_t2025, marriage_authority__secularist_reading, suppression_requirement, 2025, 0.71).
narrative_ontology:measurement_basis(marr_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__secularist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority__secularist_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% The marriage authority kernel is contested across five structurally distinct readings: secularist (this constraint), communal autonomy, federalist, gender rights, and judicial harmonization. Each reading instantiates a different ε, different beneficiary/victim structure, and different constraint type. The secularist reading differs from communal autonomy in treating pluralism as anomaly to be eliminated rather than constitutional feature; from federalism in treating majoritarian secular authority as legitimate rather than tyrannical; from gender rights in using legislative means rather than judicial expansion; and from judicial harmonization in rejecting case-by-case evolution in favor of total UCC displacement. All five readings are linked via network.affects_constraints to enable comparative analysis of how the same institutional kernel is read differently by different constitutional factions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
