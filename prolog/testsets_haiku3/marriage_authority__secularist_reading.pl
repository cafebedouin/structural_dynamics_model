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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: marriage_authority__secularist_reading
 *   human_readable: Legislative Marriage Authority and Uniform Civil Code Elimination of Personal Law Pluralism
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates the SECULARIST READING of the marriage
 *   authority kernel: a framework that lodges the legitimate authority to
 *   create and regulate family law in the democratic legislature operating
 *   under a secular constitutional order. The reading treats personal law
 *   pluralism—the institutional coexistence of multiple religious family law
 *   codes administered by state courts—as a transitional anomaly inherited
 *   from colonial divide-and-rule strategy, awaiting elimination through
 *   comprehensive Uniform Civil Code legislation. The secularist reading
 *   names the secular-modernist coalition as the primary beneficiary
 *   (legislative authority elevated, narrative of inevitable modernization
 *   established) and minority religious communities as victims (community
 *   autonomy constrained, personal law authority threatened, identity-locked
 *   stakeholders facing pressure to assimilate to secular norms). This is one
 *   of five competing readings of the marriage authority kernel; the other
 *   readings (communal_autonomy_reading, federalist_millet_reading,
 *   gender_rights_reading, judicial_harmonization_reading) each narrate
 *   different authority sources and contest the secularist claim to
 *   inevitability.
 *
 * KEY AGENTS:
 *   - Secular-modernist coalition: institutional agenda-setter, frames pluralism as relic and UCC as progress
 *   - Minority religious communities: moderate-power payers, identity-locked victims facing cultural dissolution pressure
 *   - Communal autonomy preservers: organized resistance, excluded from the framing that treats pluralism as anomaly
 *   - Democratic legislature: institutional agenda-setter, elevated as sole legitimate authority source under the secularist reading
 *   - Constitutional court: observer seat, positioned to either enforce gender-equality ceilings against personal law or defer to legislative UCC authority
 *   - Gender equality advocates: beneficiaries of the premise that secular authority can impose uniform standards, though some pursue constitutional routes instead
 *   - Minority women: powerless, trapped at intersection of community identity-lock and gender subordination; both victims and potential beneficiaries of UCC
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
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Legislative Marriage Authority and Uniform Civil Code Elimination of Personal Law Pluralism").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, '97a5dc08-139f-4132-92ac-0d931249ecee').
narrative_ontology:cs_kernel_codification('97a5dc08-139f-4132-92ac-0d931249ecee', distributed).
narrative_ontology:cs_authority_grounding('97a5dc08-139f-4132-92ac-0d931249ecee', extraction).
narrative_ontology:cs_interpretation_layer_present('97a5dc08-139f-4132-92ac-0d931249ecee').
narrative_ontology:cs_reading_relation('97a5dc08-139f-4132-92ac-0d931249ecee', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('97a5dc08-139f-4132-92ac-0d931249ecee', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('97a5dc08-139f-4132-92ac-0d931249ecee', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('97a5dc08-139f-4132-92ac-0d931249ecee', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('97a5dc08-139f-4132-92ac-0d931249ecee', foundational, legislative_supremacy_family_law).
narrative_ontology:cs_axiom_status(legislative_supremacy_family_law, holdable).
narrative_ontology:cs_axiom_grounding('97a5dc08-139f-4132-92ac-0d931249ecee', legislative_supremacy_family_law, conventional).
narrative_ontology:cs_axiom('97a5dc08-139f-4132-92ac-0d931249ecee', foundational, pluralism_as_transient_anomaly).
narrative_ontology:cs_axiom_status(pluralism_as_transient_anomaly, holdable).
narrative_ontology:cs_axiom_grounding('97a5dc08-139f-4132-92ac-0d931249ecee', pluralism_as_transient_anomaly, instrumental).
narrative_ontology:cs_axiom('97a5dc08-139f-4132-92ac-0d931249ecee', secondary, secular_law_necessary_for_modernization).
narrative_ontology:cs_axiom_status(secular_law_necessary_for_modernization, holdable).
narrative_ontology:cs_axiom_grounding('97a5dc08-139f-4132-92ac-0d931249ecee', secular_law_necessary_for_modernization, empirically_contingent).
narrative_ontology:cs_reference_frame('97a5dc08-139f-4132-92ac-0d931249ecee', secular_legislative_authority_as_sole_legitimate_source).
narrative_ontology:cs_drift_state('97a5dc08-139f-4132-92ac-0d931249ecee', contemporary_ucc_campaign_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('97a5dc08-139f-4132-92ac-0d931249ecee', '').
narrative_ontology:cs_kernel_id(marriage_authority__secularist_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, gender_equality_advocates).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, national_legal_uniformity_interest).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_religious_communities).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, communal_autonomy_preservers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, minority_women).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_women).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, secular_state_authority).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, legal_uniformity_as_modernization).
narrative_ontology:constraint_vindicates(marriage_authority__secularist_reading, democratic_legislature_as_sole_legitimate_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for a unified, secular Uniform Civil Code administered by the democratic legislature, applicable to all citizens regardless of religious affiliation. Claims personal law pluralism is a relic of colonial divide-and-rule strategy that perpetuates backward practices and prevents national legal modernization. Frames the UCC as inevitable rationalization and constitutional progress. Sets the legislative agenda, mobilizes constitutional interpretation, and drives the narrative of pluralism as a transient anomaly awaiting elimination.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, secular_modernist_coalition, agenda_setter,
    institutional, generational, analytical, national).

% Subject to dual-regime exposure: may be governed by personal law on marriage, divorce, inheritance when they invoke it, or by the secular code if they opt out or if UCC provisions override personal law via constitutional amendment or legislative action. Face the prospect of losing institutionalized access to religiously-grounded family norms and authority structures. Exit from the constraint (total reliance on secular law) requires abandoning religious legal tradition or accepting assimilation into secular norms. Coalition power is diffuse across denominations.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_religious_communities, payer,
    moderate, generational, identity_locked, national).

% Religious scholars, community leaders, and civil society organizations that defend the legitimacy and necessity of personal law pluralism as a guarantor of minority rights and cultural survival. Argue that UCC represents majoritarian domination disguised as modernization. Actively resist UCC legislation and judicial erosion of personal law scope. Excluded from the narrative framing that treats pluralism as a transitional problem rather than a deliberate consociational structure.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, communal_autonomy_preservers, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__secularist_reading, communal_autonomy_preservers, excluded).

% The institutional embodiment of the secularist reading's authority claim. Possesses formal sovereign power to unify family law through legislation. The constraint narrates legislative authority as the sole legitimate source of family law legitimacy, a reading that elevates legislature above both communal custom and constitutional courts. The legislature is simultaneously the agent of the constraint and a stakeholder whose power the constraint instrumentally relies upon.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, democratic_legislature, agenda_setter,
    institutional, civilizational, analytical, national).

% Interprets the constitutional framework and the scope of personal law authority. May enforce gender-equality guarantees against personal law codes, rein in communal authority via constitutional floor, or defer to legislative UCC authority. Occupies a structural position that can either support or resist the secularist reading depending on which constitutional principles it privileges. The current constraint describes the secularist reading's vision of how legislative authority should predominate over this interpretive function.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% Benefit from the premise that secular legislative authority can impose uniform gender equality standards across personal law regimes. Support UCC as the vehicle for eliminating intra-community gender discrimination that personal law has historically enabled. Their benefit depends on the secular reading's claim that legislature (not community) is the legitimate authority to settle family law. However, some gender advocates pursue constitutional-floor remedies (gender_rights_reading) rather than UCC, creating internal heterogeneity.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, gender_equality_advocates, beneficiary,
    organized, generational, mobile, national).

% An abstract institutional commitment to uniform law as a prerequisite for national integration, rule of law, and efficient administration. Not an actor but a vindicated proposition: the secularist reading frames legal uniformity as inherently valuable and modernizing. Benefits accrue to the state apparatus (simplified administration, unified citizenship) and to the secular modernist coalition (ideological victory).
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, national_legal_uniformity_interest, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(marriage_authority__secularist_reading, national_legal_uniformity_interest).

% Occupy the intersection of minority-community membership (trapped by identity, by exit costs, by social belonging) and gender subjection (often disadvantaged by personal law codes). The constraint offers them two paths: assimilation (exit to secular law) or voice within community reform movements. They benefit from the UCC's gender-equality promises but pay the price of community-cultural dissolution if pluralism is eliminated. Most constrained of all stakeholders.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_women, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__secularist_reading, minority_women, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves national legal uniformity in family law across a pluralistic population: replaces multiple personal law codes administered by state courts (each grounded in religious jurisprudence) with a single secular Uniform Civil Code. Solves the administrative complexity problem (courts apply different rules to different citizens based on religious affiliation), reduces legal uncertainty (citizens know which law applies), and enables uniform enforcement of constitutional principles (gender equality, secular rights) without requiring negotiation among distinct communal authorities.
% TRANSFER_FUNCTION: Moves authority over family law from dispersed religious communities and their juristic traditions to the secular democratic legislature. Transfers scope of permissible variation from broad (each community governs its own members' family law) to narrow or zero (UCC applies uniformly to all citizens). Transfers the power to define marriage, divorce, succession, inheritance, and family relationships from communal authorities and state-delegated religious courts to unified secular state institutions. Transfers the narrative legitimacy of family law from 'community self-governance in cultural matters' to 'democratic majoritarian decision-making under secular constitutional principles.'
% ABSENT_VOICES: Federalist-reading proponents who view pluralism as a constitutionally deliberate safeguard against majoritarian tyranny are not represented in the secularist framing. Conservative elements within minority communities who value personal law on intrinsic grounds (not as a compromise) are not in the room. Communal autonomy advocates who argue that state capacity for legitimate family law governance should be limited to secular matters, not extended to cultural regulation, are excluded from the narrative that treats pluralism as a relic. Judges and scholars who have developed harmonization jurisprudence (gender_rights and judicial_harmonization readings) occupy an observer role in the secularist framework, as the secularist reading insists legislative UCC is superior to case-by-case constitutional review.
% DISAPPEARANCE_RATIONALE: If the secularist reading's claim to legislative supremacy over family law disappeared (rejected in favor of federalist, communal-autonomy, or judicial-harmonization readings), the institutional and legal world would rearrange: personal law codes would retain constitutional authority, UCC legislation would be treated as overreach, the legitimacy narrative would shift from 'inevitable modernization' to 'protection of pluralism as deliberate constitutional structure,' and minority communities would reorganize around preserved personal law authority. The disappearance would be constitutionally catastrophic for the secularist coalition and administratively significant for the state apparatus (returning to pluralistic administration).
% FOUNDING_PROBLEM: Post-independence nation-states inherited colonial legal pluralism (separate personal law codes for different religious communities, administered by state courts but grounded in community jurisprudence). The secularist reading identifies this inheritance as a fundamental problem: pluralism perpetuates communal fragmentation rather than national integration, enables gender discrimination within communities, creates administrative inconsistency (courts apply different substantive law to different citizens), and prevents the nation-state from expressing a unified secular constitutional identity.
% FOUNDING_PROBLEM_CORROBORATION: The secular-modernist coalition, legislatures in India and other jurisdictions with active UCC campaigns, and certain strands of constitutional and modernization scholarship attest the founding problem is live and urgent: gender discrimination persists in personal law codes, administrative complexity is real, and legal uniformity is a prerequisite for national integration. Federalist scholars, communal autonomy advocates, comparative legal scholars studying Belgium and Malaysia, and minority-community testimony attest the problem is a misframing: legal pluralism is not a colonial relic but a deliberate constitutionalized choice for protecting minorities in plural societies, and the 'anomaly' framing serves majoritarian authority-claiming rather than describing structural legal reality. Academic works by scholars outside the secularist beneficiary set (Menski, von Benda-Beckmann, Merry, Eckert on legal pluralism; Shachar on group rights; Shah on federalism) and judicial testimony from constitutional courts in pluralistic democracies support the contested status.
narrative_ontology:disappearance_verdict(marriage_authority__secularist_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__secularist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__secularist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__secularist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__secularist_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.78) and rising over the interval (0.62→0.78) because the constraint operates through a systematic claiming of authority—the reading asserts legislative sovereignty over family law and narrates personal law as a defect to be remedied. This is not simple transfer (money, resources) but transfer of authority, legitimacy narrative, and regulatory scope. The escalating trajectory reflects intensifying pressure: early in the interval, personal law retains significant institutional space; by the end, UCC advocacy is mainstream and communal authority faces existential threat. Suppression is substantial (0.71) because maintaining this reading requires actively suppressing alternative narratives (federalist-reading legitimacy, communal-autonomy defense) and making minority exit costly (identity-locking through cultural dissolution pressure). Theater is moderate (0.42) because the genuine coordination function—resolving the administrative complexity of pluralistic systems, enabling uniform gender-equality standards—is real, but an increasing share of the measured extractiveness rides on pure narrative domination (declaring pluralism an anomaly awaiting elimination) rather than functional necessity. The measurements run on a single shared grid (all metrics authored at all time points, 0/10/20/30/40/50) to prevent temporal misalignment. Accessibility_collapse rises at the structural level (0.72→0.82) as the UCC narrative hardens into mainstream policy, but remains lower at the individual level (0.58→0.62) because minority-community members retain some capacity to organize resistance and defend personal law through judicial appeal, legislative testimony, and community mobilization.
 *
 * PERSPECTIVAL GAP:
 *   The secularist reading, from the legislative-institutional seat, appears as inevitable modernization and necessary rationalization: pluralism is incoherent, gender-equality is incompatible with community autonomy, national integration requires legal uniformity. From the minority-community seat, the same constraint appears as majoritarian domination: the legislature claims authority it historically lacked, declares plural autonomy illegitimate, and offers assimilation as the only exit. From the gender-equality seat (occupied heterogeneously), the reading offers both genuine benefit (uniform law, constitutional equality floor) and a troubling cost (community dissolution, cultural erasure). From the federalist seat, the reading represents constitutional regression: a deliberate protective mechanism (consociational pluralism) is being narrated as an artifact awaiting elimination. The engine computes per-seat directionality from the structural data: the legislative seat gets d near 0 (beneficiary), minority communities get d near 1.0 (targets), gender advocates get d near 0.3 (net benefit despite costs), federalists get excluded from the conversation entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular-modernist coalition and the democratic legislature are structural beneficiaries: they collect the narrative authority (pluralism is redefined as anomaly, legislature is elevated as sole legitimate source), gain institutional scope (UCC replaces personal law codes), and consolidate modernization framing. Directionality for these actors: d ≈ 0.0 (full beneficiary). Minority religious communities are targets: they pay in community authority loss, cultural-dissolution pressure, and identity-locking (exit from the constraint means abandoning religious legal tradition, which dissolves core identity). Directionality: d ≈ 1.0 (full target), modulated slightly upward by their organized-level power and some residual legal-pluralism defense capacity, so d ≈ 0.85–0.95. Gender-equality advocates occupy an asymmetric position: they benefit from the UCC's uniform equality standards but face a dilemma—the UCC path means majoritarian domination of minorities, while the judicial-harmonization path (gender_rights_reading) respects pluralism while imposing constitutional floors. Directionality splits: beneficiaries who support UCC get d ≈ 0.15 (substantial benefit, low cost from their seat); those pursuing constitutional routes sit at d ≈ 0.5 (symmetric, because both paths offer benefit and cost). Minority women, powerless and identity-locked, face the highest cost: they benefit nominally from uniform equality law but pay doubly (community dissolution + absorption into secular system without voice). Their directionality: d ≈ 0.95. The constitutional court, as observer, has d ≈ 0.5 (analytical seat, no extraction or subsidy).
 *
 * MANDATROPHY ANALYSIS:
 *   The secularist reading escapes the mandatrophy trap because the founding problem it names—administrative incoherence of pluralistic personal law, perpetuation of communal gender discrimination—is genuinely contested but structurally live. The secularist coalition and legislatures can point to concrete problems: women's unequal rights under some personal law codes, administrative duplication, inconsistent standards across communities. This grounds the reading as tangled_rope rather than snare: there IS a real coordination function (national legal uniformity, gender-equality enforcement) that benefits genuine constituencies. But the reading also extracts: it privileges secular modernization narratives, it forecloses federalist-reading legitimacy, and it treats minority autonomy as an anomaly to be eliminated rather than a deliberate protective structure. The extraction is high because the reading claims inevitability (UCC is progress itself) while suppressing alternatives. Mandatrophy arises when the founding problem dissolves (minorities no longer invoke personal law, gender equality is secured through constitutional floors, administrative complexity is solved without UCC), but the constraint persists. The present reading guards against this by naming the founding problem as contested—the secularist coalition would attest it is live; communal preservers and federalist scholars would attest it is a misframing. The constraint does NOT resolve mandatrophy yet, because the founding-problem-status is contested and disappearance_verdict is world_rearranges (the arrangement is consequential and contested, not atrophied).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalization_vs_ideology,
    'Is the secularist reading''s framing of pluralism as a ''transitional anomaly awaiting elimination'' an accurate description of structural legal dynamics, or an ideological narrative designed to naturalize majoritarian authority and preclude federalist-reading legitimacy?',
    'Comparative study of constitutional democracies that have preserved legal pluralism (Belgium, Malaysia, Canada, Switzerland): if pluralism persists as a stable, constitutionally-protected mechanism rather than a relic awaiting elimination, the secularist narrative is ideological, not descriptive.',
    'If the secularist framing is ideological rather than structural, the constraint reclassifies from tangled_rope (genuine coordination problem + asymmetric extraction) toward snare (pure extraction riding a constructed problem narrative). The ''founding problem'' becomes a fabrication designed to justify majority authority-claiming rather than a genuine collective-action problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_vs_ideology, conceptual, 'Whether the ''anomaly'' framing reflects legal reality or serves majoritarian authority-claiming.').

omega_variable(
    identity_locked_exit_cost,
    'For minority-community members, what is the actual cost of exit from personal law (assimilation to secular law) versus the cost of remaining under the constraint (identity-dissolution pressure from UCC advocacy)? Is the identity-locking genuine or constructed by the secularist reading itself?',
    'Ethnographic and survey data from minority communities in jurisdictions with active UCC campaigns: measure experienced identity-dissolution costs, appeal to religious law despite secular-law availability, and expressed attachment to personal law norms. Compare to jurisdictions without UCC pressure where pluralism is constitutionally secure.',
    'If identity-locking is genuine (minorities genuinely experience assimilation costs), the constraint''s suppression metric reflects real structural extraction. If identity-locking is constructed or overstated by UCC advocacy, suppression is lower than measured, and the constraint leans more toward pure-extraction (snare) classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_exit_cost, empirical, 'Whether identity-locked exit costs are structural or constructed by the secularist reading.').

omega_variable(
    gender_equality_necessity_question,
    'Is uniform secular law the necessary vehicle for gender equality in family law, or can gender-equality ceilings be enforced via constitutional interpretation across personal law codes while preserving communal autonomy?',
    'Comparative analysis of gender outcomes under three models: (1) UCC with uniform secular law, (2) constitutional floor via judicial harmonization (gender_rights_reading), (3) reformed personal law codes with statutory gender-equality amendments. Measure gender-equality outcomes in each system.',
    'If gender equality is achievable through constitutional floors and reformed personal law (as the gender_rights and judicial_harmonization readings suggest), the secularist reading''s claim that UCC is necessary dissolves. The constraint''s claimed coordination function (uniform gender equality) would be achievable without the extractive component (elimination of pluralism). This would reclassify the constraint as snare—the gender-equality benefit is real, but the UCC-means-only framing serves the secular-modernist coalition''s authority-claiming rather than the equality goal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_equality_necessity_question, empirical, 'Whether gender equality requires UCC or is achievable with constitutional floors and plural codes.').

omega_variable(
    reading_foreclosure_ambiguity,
    'Does the secularist reading''s core premise (democratic legislatures are the sole legitimate authority over family law) logically foreclose the federalist reading''s core premise (pluralism is a constitutionally deliberate protective mechanism), or do both remain logically possible within a single constitutional framework?',
    'Jurisprudential analysis: can a constitution simultaneously establish legislative supremacy over family law AND constitutionalize personal law pluralism as a protection against majoritarian domination? If yes, the readings coexist; if no, the secularist reading forecloses the federalist reading.',
    'If foreclosure is genuine, the engine computes the reading relation as ''forecloses'' and the federalist reading becomes logically impossible under secularist premises. If coexistence is logically possible (different parties hold different readings of the same text), the relation is ''coexists_with'' and both remain live. This affects the terminal attractor for the kernel contest: foreclosure means one reading will eventually eliminate the other; coexistence means the contest persists as a live institutional conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_ambiguity, conceptual, 'Whether the secularist and federalist readings logically foreclose each other or logically coexist.').

omega_variable(
    communal_autonomy_reading_kinship,
    'Is the secularist reading genuinely zero-sum with the communal_autonomy_reading, or do both claim to ground their authority in state legitimacy (the communal reading argues the state legitimately delegates authority to communities, while the secularist reading argues the state legitimately reclaims it)?',
    'Textual analysis of how each reading justifies state involvement: does the communal reading require state authority to delegate to communities, or does it claim community authority exists independently of state? Does the secularist reading claim it can unilaterally revoke delegation, or does it require negotiated transfer of power?',
    'If both readings depend on state legitimacy and authority, they are competing narratives about the same state authority distribution, not logically foreclosing each other. This supports a ''coexists_with'' relation. If the communal reading claims purely indigenous community authority independent of state, then the readings do foreclose (either state legitimates community law, or state monopolizes authority).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(communal_autonomy_reading_kinship, conceptual, 'Whether the secularist reading logically forecloses or coexists with the communal_autonomy reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__secularist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__secularist_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__secularist_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(marr_tr_t20, observed).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__secularist_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(marr_tr_t30, observed).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__secularist_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(marr_tr_t40, projected).
narrative_ontology:measurement(marr_tr_t50, marriage_authority__secularist_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(marr_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__secularist_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t10, marriage_authority__secularist_reading, base_extractiveness, 10, 0.67).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t20, marriage_authority__secularist_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement_basis(marr_be_t20, observed).
narrative_ontology:measurement(marr_be_t30, marriage_authority__secularist_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement_basis(marr_be_t30, observed).
narrative_ontology:measurement(marr_be_t40, marriage_authority__secularist_reading, base_extractiveness, 40, 0.77).
narrative_ontology:measurement_basis(marr_be_t40, projected).
narrative_ontology:measurement(marr_be_t50, marriage_authority__secularist_reading, base_extractiveness, 50, 0.78).
narrative_ontology:measurement_basis(marr_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__secularist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t10, marriage_authority__secularist_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t20, marriage_authority__secularist_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(marr_su_t20, observed).
narrative_ontology:measurement(marr_su_t30, marriage_authority__secularist_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement_basis(marr_su_t30, observed).
narrative_ontology:measurement(marr_su_t40, marriage_authority__secularist_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(marr_su_t40, projected).
narrative_ontology:measurement(marr_su_t50, marriage_authority__secularist_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(marr_su_t50, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(marr_grid_01, marriage_authority__secularist_reading, accessibility_collapse(class), 0, 0.68).
narrative_ontology:measurement(marr_grid_02, marriage_authority__secularist_reading, accessibility_collapse(class), 50, 0.75).
narrative_ontology:measurement(marr_grid_03, marriage_authority__secularist_reading, accessibility_collapse(individual), 0, 0.58).
narrative_ontology:measurement(marr_grid_04, marriage_authority__secularist_reading, accessibility_collapse(individual), 50, 0.62).
narrative_ontology:measurement(marr_grid_05, marriage_authority__secularist_reading, accessibility_collapse(organizational), 0, 0.65).
narrative_ontology:measurement(marr_grid_06, marriage_authority__secularist_reading, accessibility_collapse(organizational), 50, 0.76).
narrative_ontology:measurement(marr_grid_07, marriage_authority__secularist_reading, accessibility_collapse(structural), 0, 0.72).
narrative_ontology:measurement(marr_grid_08, marriage_authority__secularist_reading, accessibility_collapse(structural), 50, 0.82).
narrative_ontology:measurement(marr_grid_09, marriage_authority__secularist_reading, resistance(class), 0, 0.71).
narrative_ontology:measurement(marr_grid_10, marriage_authority__secularist_reading, resistance(class), 50, 0.64).
narrative_ontology:measurement(marr_grid_11, marriage_authority__secularist_reading, resistance(individual), 0, 0.58).
narrative_ontology:measurement(marr_grid_12, marriage_authority__secularist_reading, resistance(individual), 50, 0.52).
narrative_ontology:measurement(marr_grid_13, marriage_authority__secularist_reading, resistance(organizational), 0, 0.74).
narrative_ontology:measurement(marr_grid_14, marriage_authority__secularist_reading, resistance(organizational), 50, 0.68).
narrative_ontology:measurement(marr_grid_15, marriage_authority__secularist_reading, resistance(structural), 0, 0.68).
narrative_ontology:measurement(marr_grid_16, marriage_authority__secularist_reading, resistance(structural), 50, 0.62).
narrative_ontology:measurement(marr_grid_17, marriage_authority__secularist_reading, stakes_inflation(class), 0, 0.65).
narrative_ontology:measurement(marr_grid_18, marriage_authority__secularist_reading, stakes_inflation(class), 50, 0.72).
narrative_ontology:measurement(marr_grid_19, marriage_authority__secularist_reading, stakes_inflation(individual), 0, 0.58).
narrative_ontology:measurement(marr_grid_20, marriage_authority__secularist_reading, stakes_inflation(individual), 50, 0.64).
narrative_ontology:measurement(marr_grid_21, marriage_authority__secularist_reading, stakes_inflation(organizational), 0, 0.72).
narrative_ontology:measurement(marr_grid_22, marriage_authority__secularist_reading, stakes_inflation(organizational), 50, 0.78).
narrative_ontology:measurement(marr_grid_23, marriage_authority__secularist_reading, stakes_inflation(structural), 0, 0.68).
narrative_ontology:measurement(marr_grid_24, marriage_authority__secularist_reading, stakes_inflation(structural), 50, 0.76).
narrative_ontology:measurement(marr_grid_25, marriage_authority__secularist_reading, suppression(class), 0, 0.62).
narrative_ontology:measurement(marr_grid_26, marriage_authority__secularist_reading, suppression(class), 50, 0.72).
narrative_ontology:measurement(marr_grid_27, marriage_authority__secularist_reading, suppression(individual), 0, 0.48).
narrative_ontology:measurement(marr_grid_28, marriage_authority__secularist_reading, suppression(individual), 50, 0.56).
narrative_ontology:measurement(marr_grid_29, marriage_authority__secularist_reading, suppression(organizational), 0, 0.58).
narrative_ontology:measurement(marr_grid_30, marriage_authority__secularist_reading, suppression(organizational), 50, 0.68).
narrative_ontology:measurement(marr_grid_31, marriage_authority__secularist_reading, suppression(structural), 0, 0.62).
narrative_ontology:measurement(marr_grid_32, marriage_authority__secularist_reading, suppression(structural), 50, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__secularist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority__secularist_reading, 0.14).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% The marriage authority kernel decomposes into five structurally distinct constraint stories, each instantiating a different reading of who legitimately authors family law. The secularist reading narrates legislative supremacy and treats pluralism as an anomaly; the communal_autonomy reading narrates community self-governance delegated by the state; the federalist reading treats pluralism as a constitutionally deliberate protection against majoritarianism; the gender_rights reading uses constitutional equality to impose floors across personal law codes; the judicial_harmonization reading pursues incremental equalization via Supreme Court review. Each reading has distinct ε (extractiveness of the standing arrangement from that reading's perspective), distinct beneficiary/victim structures, and distinct claim-type. They are linked as a constraint family via affects_constraints and compete for dominance in the institutional and narrative landscape.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__secularist_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
