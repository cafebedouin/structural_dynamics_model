% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__parsi_communal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__parsi_communal_reading, []).

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
 *   constraint_id: marriage_authority_kernel__parsi_communal_reading
 *   human_readable: Parsi Marriage and Divorce Act 1936 — Communal Tribunal Authority
 *   domain: comparative_law/religious_governance/family_law
 *
 * SUMMARY:
 *   The Parsi Marriage and Divorce Act 1936 codifies a parallel matrimonial
 *   court system administered by community panchayats (a jury-style
 *   'Delegates' court) for India's Zoroastrian Parsi minority, formalizing
 *   customary self-governance that predates codification. The arrangement
 *   genuinely solves a coordination problem for a small, geographically
 *   dispersed community: it provides community-specific divorce grounds, a
 *   familiar adjudicatory forum, and continuity with pre-colonial customary
 *   practice. But the same institutional structure that administers this
 *   coordination also polices a strict, and gender-asymmetric, endogamy
 *   boundary — women who marry outside the community risk loss of ritual and
 *   trust access in ways men do not — while the community's overall
 *   population has nearly halved since 1941, partly as a consequence of the
 *   very boundary-enforcement the panchayat maintains.
 *
 * KEY AGENTS:
 *   - parsi_panchayat_trustees: agenda_setter (institutional/arbitrage) — administers matrimonial courts and communal resource access
 *   - parsi_priesthood: beneficiary (organized/constrained) — holds ritual monopoly protected by the endogamy boundary
 *   - parsi_women_married_out: payer (moderate/trapped) — bears gendered loss of standing under asymmetric custom
 *   - children_of_intermarriage: payer (powerless/trapped) — inherit contested recognition status with no voice in the process
 *   - indian_constitutional_courts: observer (institutional/analytical) — adjudicate rights claims against communal self-governance claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, 0.38).
domain_priors:suppression_score(marriage_authority_kernel__parsi_communal_reading, 0.52).
domain_priors:theater_ratio(marriage_authority_kernel__parsi_communal_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__parsi_communal_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__parsi_communal_reading, "Parsi Marriage and Divorce Act 1936 — Communal Tribunal Authority").
narrative_ontology:topic_domain(marriage_authority_kernel__parsi_communal_reading, "comparative_law/religious_governance/family_law").

domain_priors:requires_active_enforcement(marriage_authority_kernel__parsi_communal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__parsi_communal_reading, '84fed644-9dd1-4cc0-a0da-e1e4651a802b').
narrative_ontology:cs_kernel_codification('84fed644-9dd1-4cc0-a0da-e1e4651a802b', formalized).
narrative_ontology:cs_authority_grounding('84fed644-9dd1-4cc0-a0da-e1e4651a802b', lineage).
narrative_ontology:cs_interpretation_layer_present('84fed644-9dd1-4cc0-a0da-e1e4651a802b').
narrative_ontology:cs_reading_relation('84fed644-9dd1-4cc0-a0da-e1e4651a802b', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('84fed644-9dd1-4cc0-a0da-e1e4651a802b', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('84fed644-9dd1-4cc0-a0da-e1e4651a802b', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('84fed644-9dd1-4cc0-a0da-e1e4651a802b', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('84fed644-9dd1-4cc0-a0da-e1e4651a802b', foundational, communal_descent_boundary_defines_membership).
narrative_ontology:cs_axiom_status(communal_descent_boundary_defines_membership, holdable).
narrative_ontology:cs_axiom_grounding('84fed644-9dd1-4cc0-a0da-e1e4651a802b', communal_descent_boundary_defines_membership, conventional).
narrative_ontology:cs_axiom('84fed644-9dd1-4cc0-a0da-e1e4651a802b', foundational, community_tribunal_jurisdiction_supersedes_general_civil_forum).
narrative_ontology:cs_axiom_status(community_tribunal_jurisdiction_supersedes_general_civil_forum, holdable).
narrative_ontology:cs_axiom_grounding('84fed644-9dd1-4cc0-a0da-e1e4651a802b', community_tribunal_jurisdiction_supersedes_general_civil_forum, conventional).
narrative_ontology:cs_axiom('84fed644-9dd1-4cc0-a0da-e1e4651a802b', secondary, endogamy_preservation_justifies_gender_differentiated_recognition).
narrative_ontology:cs_axiom_status(endogamy_preservation_justifies_gender_differentiated_recognition, holdable).
narrative_ontology:cs_axiom_grounding('84fed644-9dd1-4cc0-a0da-e1e4651a802b', endogamy_preservation_justifies_gender_differentiated_recognition, instrumental).
narrative_ontology:cs_reference_frame('84fed644-9dd1-4cc0-a0da-e1e4651a802b', colonial_era_panchayat_customary_jurisdiction).
narrative_ontology:cs_drift_state('84fed644-9dd1-4cc0-a0da-e1e4651a802b', contemporary_demographic_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('84fed644-9dd1-4cc0-a0da-e1e4651a802b', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_panchayat_trustees).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, endogamous_community_institutions).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_priesthood).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_women_married_out).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, children_of_intermarriage).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, converts_seeking_recognition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_men_married_out).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, demographically_declining_community).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, demographically_declining_community).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, communal_self_governance_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, endogamy_preserves_communal_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the special Parsi matrimonial courts (delegates and jury of five appointed under the Act), controls access to community trust funds, fire temples, and the Tower of Silence, and adjudicates matrimonial disputes under a parallel court structure recognized by the Indian state. Sets the practical boundary of who counts as Parsi for marriage-registration purposes.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_panchayat_trustees, agenda_setter,
    institutional, generational, arbitrage, national).

% Performs the navjote and Zoroastrian wedding rites that the Act's framework treats as constitutive of a recognized Parsi marriage. Their ritual monopoly is protected by the same endogamy rule that keeps the community's religious economy self-contained; loosening the boundary threatens their institutional role, not merely doctrine.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_priesthood, beneficiary,
    organized, generational, constrained, national).

% Trusts, housing colonies (baugs), schools, and charitable funds restricted to Parsis in good communal standing benefit from a narrow, patrilineally-policed definition of who is Parsi. A tighter boundary preserves resource allocation within a shrinking pool of eligible beneficiaries.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, endogamous_community_institutions, beneficiary,
    organized, civilizational, constrained, national).

% Under long-standing communal practice (and contested case law, e.g. Goolrukh Gupta), a Parsi woman who marries outside the community risks losing access to Tower of Silence rites, trust benefits, and full community standing for herself and her children — while a Parsi man marrying out does not lose equivalent standing, and his children are typically still recognized as Parsi. She cannot simply litigate her way to parity without years of tribunal and civil-court contest, and exit from the community deprives her of the very goods (funeral rites, trust access, identity) the constraint governs.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_women_married_out, payer,
    moderate, biographical, trapped, national).

% Children born to a Parsi mother and non-Parsi father face disputed and inconsistently applied recognition as Parsi under the community's patrilineal custom, affecting navjote eligibility, marriage registration under the Act, and access to communal trusts and funeral rites. They have no seat in the panchayat process that decides their status and inherit the consequences of a rule they had no part in making.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, children_of_intermarriage, payer,
    powerless, biographical, trapped, national).

% Non-Parsis who wish to convert to Zoroastrianism (or whose parent converted) are, in most panchayat readings, categorically barred from recognition regardless of sincerity or practice, because the community's self-definition is treated as descent-based rather than faith-based. They have no forum inside the community structure to press their claim; they are simply outside the boundary the constraint draws.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, converts_seeking_recognition, excluded,
    powerless, biographical, trapped, national).

% A Parsi man who marries a non-Parsi woman typically retains full communal standing and his children are usually recognized as Parsi, illustrating the asymmetry within the same nominal community-member category. His exit options from any adverse consequence are functionally open in a way his female counterpart's are not.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_men_married_out, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, parsi_men_married_out, observer).

% Adjudicate constitutional challenges (equality under Article 14/15, personal liberty under Article 21) to the panchayat's gendered application of endogamy custom, weighing communal self-governance and religious-freedom claims (Article 26) against individual-rights claims by women and children affected by the custom. Their rulings can narrow or entrench the panchayat's practical authority without abolishing the Act itself.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, indian_constitutional_courts, observer,
    institutional, generational, analytical, national).

% The Parsi population in India has fallen from roughly 114,000 in 1941 to under 60,000 today, driven partly by the strict endogamy the constraint enforces (low intermarriage-recognition, low fertility, emigration). The same boundary-maintenance mechanism that preserves communal identity and resource concentration for present trustees accelerates the demographic decline that threatens the community's long-term viability — the constraint is simultaneously self-preserving and self-undermining across different time horizons.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, demographically_declining_community, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, demographically_declining_community, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__parsi_communal_reading, parsi_panchayat_trustees).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__parsi_communal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a self-administered matrimonial court system (special delegates under the 1936 Act) that lets a small, geographically dispersed religious minority resolve marriage, divorce, and maintenance disputes according to its own customary norms rather than through general civil courts, and coordinates access to communal religious and charitable resources (fire temples, Towers of Silence, trusts) around a stable, verifiable membership boundary.
% TRANSFER_FUNCTION: Moves control over communal identity-recognition, trust access, and death/marriage ritual eligibility from individual community members (especially women who marry out and their children) to the panchayat trustees and priesthood who administer the boundary; the asymmetric gendered rule specifically transfers standing away from exogamous women and their children toward the institutions that police endogamy.
% ABSENT_VOICES: Children of intermarriage have no vote or standing in the panchayat process that decides their communal status. Would-be converts are excluded from any recognition forum entirely. Parsi women's associations (e.g. groups that litigated Goolrukh Gupta) have raised objection through the constitutional courts precisely because the internal community forum offers them no path to parity.
% DISAPPEARANCE_RATIONALE: If the Act's special court structure and the customary endogamy rule it codifies vanished overnight, Parsi matrimonial disputes would default to the Special Marriage Act or general civil family courts, trust and religious-site access would need new (likely more inclusive or more contested) eligibility criteria, the panchayat's practical governing role would collapse, and the gendered exclusion of women married out would lose its primary institutional mechanism — a substantial rearrangement of communal life, resource allocation, and the demographic trajectory of the community itself.
% FOUNDING_PROBLEM: In 1936, colonial-era general matrimonial law did not fit Parsi customary practice (e.g., the panchayat jury-of-five adjudication model, community-specific grounds for divorce, and the community's desire for self-administered courts rather than British ecclesiastical or civil forums), and Parsis sought a codified statute that would formalize their existing customary tribunal system with state backing.
% FOUNDING_PROBLEM_CORROBORATION: Panchayat trustees and priesthood attest the founding problem is still live: a small, geographically dispersed, demographically fragile community still needs self-administered courts to survive as a distinct religious-cultural unit. Constitutional court rulings (Goolrukh Gupta v. Burjor Pardiwala, Bombay High Court and subsequent litigation) and independent demographic researchers attest that the mechanism, particularly the gendered endogamy rule, now functions less to solve a genuine adjudicatory-forum problem and more to police a shrinking membership boundary in ways that accelerate the community's decline — a status contested between the administering institution and outside constitutional and demographic observers.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__parsi_communal_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__parsi_communal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__parsi_communal_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__parsi_communal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__parsi_communal_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__parsi_communal_reading_tests).
:- end_tests(marriage_authority_kernel__parsi_communal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) is moderate, not high: the panchayat court system performs a genuine, low-theater coordination function (dispute resolution, ritual/administrative continuity) for most community members, so this is not primarily a rent-extraction structure. But it is not zero, because the gendered application of the endogamy rule transfers standing asymmetrically from women who marry out (and their children) to the institutions maintaining the boundary — a real, if narrow, extractive channel running through the same structure that does the coordinating. Suppression (0.52) is moderate-high because exit from the community forfeits identity-constitutive goods (funeral rites, trust access) that have no full substitute outside the community, which is a meaningful barrier even without formal coercion. Theater ratio is low (0.22) and rises only slowly — the tribunal function remains substantially real across the interval, distinguishing this from a pure-performance piton. Accessibility collapse (0.58) reflects that alternatives (civil courts, Special Marriage Act) exist in principle but carry a real cost — loss of communal identity and resources — that most members do not treat as a live option. Resistance (0.44) reflects organized internal and constitutional-court pushback specifically against the gendered asymmetry, not against the tribunal system as a whole.
 *
 * PERSPECTIVAL GAP:
 *   From the panchayat trustee seat, the arrangement is a coordination mechanism preserving communal continuity against demographic pressure that makes every boundary decision existentially weighted. From the seat of a Parsi woman married outside the community, the identical rule computes as an enforced asymmetric cost she cannot litigate away without years of contest and which her male counterpart never faces. The engine should compute these as different types from the same structural data — the trustee's arbitrage-grade exit and institutional power sit far from the trapped, moderate-power position of the woman bearing the gendered cost, and that positional gap is exactly what the directionality derivation is built to capture without either seat's narrative being privileged as 'the' truth about the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Panchayat trustees and priesthood sit near the beneficiary end: they administer the boundary, control resource access, and their institutional role depends on the endogamy rule's continued enforcement. Parsi women married out and their children sit near the target end: they bear a cost (loss of standing, contested recognition) that similarly-situated men do not bear, with trapped exit because leaving the community forfeits the goods the constraint governs rather than escaping a burden. Converts are excluded entirely rather than coordinated — the constraint's boundary-definition is drawn to keep them structurally outside any forum. The demographically-declining-community seat carries a dual directionality: present trustees and institutions benefit from present boundary maintenance, but the community as a whole, across a longer time horizon, pays a viability cost from the same mechanism — this is why the same agent-type appears as both payer and beneficiary depending on time horizon, and is exactly the kind of asymmetry an override would be needed for if the derivation collapsed it to a single value (no override authored here because the dual-role stakeholder object already carries the distinction).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for a community-administered forum fitting Parsi customary practice, unserved by 1936 colonial civil/ecclesiastical courts) remains partially live — small, dispersed religious minorities still benefit from culturally-fitted adjudication — which is why founding_problem_status is 'contested' rather than 'dead'. This prevents the classification from collapsing to pure snare: unlike an arrangement whose founding function has fully disappeared while extraction persists, the Parsi panchayat courts still perform real, low-theater dispute resolution for most matrimonial matters. What has drifted is narrower and more identifiable: the gendered application of endogamy enforcement, which now operates less as boundary-preservation-for-community-continuity and more as an asymmetric cost imposed on exogamous women specifically, in a demographic environment where that same boundary-enforcement measurably shrinks the community it purports to protect. Tangled Rope, not snare, is the correct claim because both the coordination function (real, ongoing, low-theater) and the asymmetric extraction (real, identifiable victims, requires active tribunal enforcement) are simultaneously present and neither substitutes for the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_framing_kernel_reading_selection,
    'Is grounding marriage/family authority in Parsi community custom the correct or only defensible reading of the marriage_authority_kernel for this population, or does the secular_civil_reading (Special Marriage Act, constitutional individual rights) represent an equally valid alternative framing that this constraint''s classification suppresses by construction?',
    'Track constitutional court outcomes (e.g. Goolrukh Gupta and successor litigation) that adjudicate the boundary between communal self-governance claims (Article 26) and individual constitutional rights claims (Articles 14, 15, 21) for the same fact pattern; if courts systematically favor the individual-rights reading over the communal reading in future cases, that is evidence the kernel''s center of gravity has shifted toward the secular_civil_reading.',
    'If the secular_civil_reading is adopted as controlling by future courts, this reading''s tribunal authority becomes structurally subordinate rather than co-equal, which would shift this reading''s effective classification toward scaffold (transitional accommodation) rather than tangled_rope (stable hybrid coordination/extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_kernel_reading_selection, conceptual, 'Whether the communal reading is a co-equal kernel framing or a subordinate accommodation within the larger secular constitutional order.').

omega_variable(
    sibling_reading_relationship_gendered_asymmetry,
    'Does the gendered endogamy asymmetry documented in this reading (men marrying out retain standing, women marrying out typically do not) also appear structurally in the hindu_codified_reading and muslim_shariat_reading siblings, or is it a distinguishing feature specific to Parsi custom''s demographic anxiety?',
    'Comparative review of each sibling reading''s own base_properties.victims declarations and gender-differentiated exit_options for equivalent stakeholder classes (women married out of each respective community).',
    'If the asymmetry is common across siblings, the marriage_authority_kernel itself carries a shared gendered-extraction axis independent of which reading is instantiated; if unique to this reading, the demographic-decline framing specific to a small, shrinking community is the causal driver rather than a general feature of religious personal-law systems in India.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_relationship_gendered_asymmetry, empirical, 'Whether gendered endogamy extraction is kernel-wide or specific to this reading''s demographic context.').

omega_variable(
    demographic_decline_causal_weight,
    'How much of the Parsi population decline (1941: ~114,000 to 2026: under 60,000) is causally attributable to the endogamy-recognition rule itself, versus independent factors (low fertility preference, emigration, urbanization) that would operate regardless of the rule?',
    'Demographic modeling comparing recognition-rule liberalization outcomes (e.g. jurisdictions or sub-communities that have relaxed patrilineal recognition) against control populations; longitudinal tracking of intermarriage-recognition reform proposals and their demographic effects if adopted.',
    'If the rule is a major causal driver, the tangled_rope classification is under-stating urgency — the extraction is accelerating the destruction of the very coordination good (community continuity) it claims to protect, which would push toward reclassification as a self-defeating tangled_rope approaching involuntary scaffold status. If a minor driver, the demographic framing is better read as omega-level uncertainty rather than a a core structural fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demographic_decline_causal_weight, empirical, 'Causal weight of the endogamy rule in the community''s demographic decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__parsi_communal_reading, 1936, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1936, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1936, 0.1).
narrative_ontology:measurement_basis(marr_tr_t1936, observed).
narrative_ontology:measurement(marr_tr_t1954, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1954, 0.12).
narrative_ontology:measurement_basis(marr_tr_t1954, observed).
narrative_ontology:measurement(marr_tr_t1980, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement_basis(marr_tr_t1980, observed).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement_basis(marr_tr_t2000, observed).
narrative_ontology:measurement(marr_tr_t2012, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2012, 0.2).
narrative_ontology:measurement_basis(marr_tr_t2012, observed).
narrative_ontology:measurement(marr_tr_t2026, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2026, 0.22).
narrative_ontology:measurement_basis(marr_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(marr_be_t1936, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1936, 0.22).
narrative_ontology:measurement_basis(marr_be_t1936, observed).
narrative_ontology:measurement(marr_be_t1954, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1954, 0.26).
narrative_ontology:measurement_basis(marr_be_t1954, observed).
narrative_ontology:measurement(marr_be_t1980, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement_basis(marr_be_t1980, observed).
narrative_ontology:measurement(marr_be_t2000, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement_basis(marr_be_t2000, observed).
narrative_ontology:measurement(marr_be_t2012, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2012, 0.36).
narrative_ontology:measurement_basis(marr_be_t2012, observed).
narrative_ontology:measurement(marr_be_t2026, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2026, 0.38).
narrative_ontology:measurement_basis(marr_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1936, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1936, 0.4).
narrative_ontology:measurement_basis(marr_su_t1936, observed).
narrative_ontology:measurement(marr_su_t1954, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1954, 0.42).
narrative_ontology:measurement_basis(marr_su_t1954, observed).
narrative_ontology:measurement(marr_su_t1980, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement_basis(marr_su_t1980, observed).
narrative_ontology:measurement(marr_su_t2000, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement_basis(marr_su_t2000, observed).
narrative_ontology:measurement(marr_su_t2012, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2012, 0.5).
narrative_ontology:measurement_basis(marr_su_t2012, observed).
narrative_ontology:measurement(marr_su_t2026, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2026, 0.52).
narrative_ontology:measurement_basis(marr_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__parsi_communal_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the marriage_authority_kernel, each claiming legitimate authority over marriage/family law from a different source (community custom, codified statute, religious jurisprudence, canonical law, secular constitutional code). Each reading has its own ε, beneficiary/victim structure, and classification; they are not the same constraint viewed from different angles but structurally distinct constraints sharing a contested kernel. This reading's distinguishing delta: community-administered (not state civil-court) tribunals, formally high internal gender-equity aspiration alongside gendered endogamy-enforcement practice, and demographic decline that makes the coordination function's long-run viability questionable in a way none of the sibling readings' populations face at comparable scale.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
