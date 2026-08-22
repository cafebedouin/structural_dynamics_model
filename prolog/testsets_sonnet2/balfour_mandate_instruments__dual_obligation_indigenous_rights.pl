% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__dual_obligation_indigenous_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__dual_obligation_indigenous_rights, []).

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
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Mandate Dual-Obligation Reading: Arab Civil/Political Rights and Land Tenure Protection Subordinating the National Home
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   Between 1920 and 1939, Palestinian Arab communities, elites, and their
 *   legal advocates pressed a specific reading of the Mandate for Palestine:
 *   that its protective language for 'the rights and position of other
 *   sections of the population' was not decorative but created a binding
 *   obligation, coordinate with or superior to the national home clause, to
 *   preserve existing Arab civil and political rights and land tenure. On
 *   this reading, land transfer ordinances (culminating in the 1940
 *   regulations, foreshadowed by the 1930-31 Passfield/Hope-Simpson findings)
 *   and immigration quota ceilings (the 1939 White Paper) were not policy
 *   concessions but enforcement of a standing legal duty. This reading treats
 *   Arab demographic majority as grounding a path to representative
 *   government and eventual sovereignty under League self-determination
 *   norms, and treats the national home commitment as subordinate to that
 *   duty wherever the two conflict.
 *
 * KEY AGENTS:
 *   - palestinian_arab_communities: primary beneficiary (moderate/constrained) — protected tenure and political status
 *   - palestinian_arab_elites: agenda-setting beneficiary (organized/constrained) — supplies the interpretive argument grounding the reading
 *   - zionist_organizations: primary target (organized/constrained) — land and immigration restricted
 *   - british_mandatory_administrators: institutional payer/agenda-setter (institutional/constrained) — bears enforcement cost of cross-pressured duty
 *   - permanent_mandates_commission: analytical observer (institutional/analytical) — oversees compliance with the dual obligation
 *   - rural_arab_tenant_cultivators: excluded population — nominal protection subject, no independent voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.71).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.6).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.71).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Mandate Dual-Obligation Reading: Arab Civil/Political Rights and Land Tenure Protection Subordinating the National Home").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'a53e0796-610f-4db6-8e3d-32317a2a7322').
narrative_ontology:cs_kernel_codification('a53e0796-610f-4db6-8e3d-32317a2a7322', fixed_text).
narrative_ontology:cs_authority_grounding('a53e0796-610f-4db6-8e3d-32317a2a7322', lineage).
narrative_ontology:cs_interpretation_layer_present('a53e0796-610f-4db6-8e3d-32317a2a7322').
narrative_ontology:cs_reading_relation('a53e0796-610f-4db6-8e3d-32317a2a7322', balfour_mandate_instruments__jewish_national_home_primacy, coexists_with).
narrative_ontology:cs_reading_relation('a53e0796-610f-4db6-8e3d-32317a2a7322', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('a53e0796-610f-4db6-8e3d-32317a2a7322', foundational, existing_majority_rights_bind_prior_political_commitment).
narrative_ontology:cs_axiom_status(existing_majority_rights_bind_prior_political_commitment, holdable).
narrative_ontology:cs_axiom_grounding('a53e0796-610f-4db6-8e3d-32317a2a7322', existing_majority_rights_bind_prior_political_commitment, deontological).
narrative_ontology:cs_axiom('a53e0796-610f-4db6-8e3d-32317a2a7322', foundational, self_determination_norm_governs_mandate_interpretation).
narrative_ontology:cs_axiom_status(self_determination_norm_governs_mandate_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('a53e0796-610f-4db6-8e3d-32317a2a7322', self_determination_norm_governs_mandate_interpretation, conventional).
narrative_ontology:cs_reference_frame('a53e0796-610f-4db6-8e3d-32317a2a7322', covenant_article_22_sacred_trust_framework).
narrative_ontology:cs_drift_state('a53e0796-610f-4db6-8e3d-32317a2a7322', post_1929_disturbances_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a53e0796-610f-4db6-8e3d-32317a2a7322', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administrators).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, self_determination_norm_primacy).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, minority_protection_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Existing cultivators and smallholders whose tenure the mandate's protective clauses (Article 6's 'rights and position of other sections of the population' language, land transfer ordinances) are read to shield from displacement. Their claim rests on continuous occupation and demographic majority; the reading treats their existing civil and political status as the baseline the mandate must not degrade, not as a variable subject to bargaining with an incoming settler population.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities, beneficiary,
    moderate, generational, constrained, regional).

% Notable families, municipal councils, and the Arab Executive petition the Permanent Mandates Commission and British Parliament directly, invoking self-determination norms under the League Covenant's Article 22 and the developing minority-protection treaty regime. They administer no formal apparatus but set the interpretive agenda for this reading by supplying the legal argument that majority status grounds a sovereignty path; their exit option is constrained because appeal runs only through the same mandatory and League channels that also empower the rival reading.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites, agenda_setter).

% The Jewish Agency and affiliated bodies experience land transfer restrictions and immigration quotas as caps on demographic parity and land acquisition that this reading requires to protect Arab tenure. Their diplomatic leverage runs through the same League and British channels the dual-obligation reading also occupies, so they cannot simply exit to an alternative forum; every quota tightening or land ordinance under this reading is a direct constraint on their stated program.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations, payer,
    organized, generational, constrained, global).

% Colonial Office and Palestine administration officials who must simultaneously satisfy the Balfour Declaration's textual reference to a Jewish national home and this reading's requirement (drawn from the same mandate instrument's protective clauses and League oversight) that existing Arab rights and land tenure not be prejudiced. Under this reading their discretion to accommodate Zionist demands is bounded by an equal or superior obligation, producing administrative paralysis (White Papers, land commissions, shifting quota formulas) that they experience as a binding constraint rather than a policy choice.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administrators, payer,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administrators, agenda_setter).

% The League of Nations oversight body receives annual reports and petitions from all parties, assessing British administration against the mandate's dual textual commitments. It has no direct enforcement power but its findings feed the legitimacy contest between readings; under this reading its role is to hold Britain to the protective obligation as a matter of self-determination and minority-protection law.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, permanent_mandates_commission, observer,
    institutional, generational, analytical, global).

% Fellahin and tenant farmers whose land is transferred by absentee landlords to Jewish settlement companies are the population the protective reading claims to shield, but they petition through no formal channel of their own — their interests are represented, filtered, and sometimes overridden by the Arab elite negotiating position and by British land commissions, neither of which they control.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, rural_arab_tenant_cultivators, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__dual_obligation_indigenous_rights, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The mandate instrument, on this reading, coordinates a genuine legal problem: how a temporary international trusteeship exercising sovereign-like administrative power over an existing majority population can proceed without extinguishing that population's pre-existing civil, political, and property rights — the League Covenant's Article 22 self-determination framework and contemporaneous minority-protection treaties supply the coordinating norm.
% TRANSFER_FUNCTION: Under this reading the arrangement transfers political leverage and land-tenure security toward the existing Arab population and away from Zionist demographic and territorial ambition: land transfer ordinances and immigration quota ceilings move acquisition opportunity and settlement capacity from Zionist organizations to the protected status quo population, while binding British administrators to enforce that transfer against their competing textual obligation to facilitate the national home.
% ABSENT_VOICES: Rural Arab tenant cultivators, whose actual land tenure is at stake, are represented only through Arab elite intermediaries and British land commissions; they hold no independent petition channel to the Permanent Mandates Commission and their displacement experience does not appear directly in the diplomatic record this reading is built from.
% DISAPPEARANCE_RATIONALE: If the protective obligation were read out of the mandate instrument entirely, land transfer restrictions and immigration ceilings would lose their legal grounding, removing the primary check this reading places on Zionist land acquisition and demographic parity — British administrators would lose their claimed justification for restrictive ordinances, and Arab elite claims to a sovereignty path grounded in majority status and self-determination norms would lose their textual anchor in the instrument itself.
% FOUNDING_PROBLEM: The League of Nations mandate system was built to reconcile the Covenant's stated principle that mandated peoples' 'well-being and development form a sacred trust of civilisation' — including eventual self-determination for communities capable of it — with Britain's prior political commitment (the Balfour Declaration) to facilitate a Jewish national home in the same territory, without either commitment simply erasing the other.
% FOUNDING_PROBLEM_CORROBORATION: The Permanent Mandates Commission's own session minutes and the Arab Executive's petitions attest that the protective obligation was treated as real and binding by League overseers independent of Arab or Zionist interest, not merely as a public-relations gloss; the Jewish Agency and its later historiography attest the opposite, that the protective language was subordinate to the national home mandate and was administratively deprioritized whenever it conflicted with settlement facilitation — corroboration outside both benefiting parties is thin, since the League itself dissolved before any final adjudication was reached.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__dual_obligation_indigenous_rights, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__dual_obligation_indigenous_rights, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.35 to 0.71) because the reading's coordination function — protecting existing tenure and political status — increasingly requires active restriction of Zionist land acquisition and immigration, which is experienced by Zionist organizations and by British administrators (torn between two textual obligations) as extraction from their stated interests. Suppression tracks the intensification of British enforcement machinery (land commissions, quota administration, and ultimately the 1936-39 Arab Revolt's suppression) needed to hold the arrangement against resistance from the rival reading's proponents, peaking during the Revolt (1936) before partial British retreat from Zionist facilitation lowers it slightly by 1939. Theater ratio starts elevated (0.55) reflecting genuine ambiguity about whether the protective clauses would be enforced at all, and falls somewhat as enforcement becomes concretely institutionalized in land ordinances and the White Paper — though it never falls to near-zero because much of the protective apparatus (commissions, inquiries) produced reports without binding follow-through.
 *
 * PERSPECTIVAL GAP:
 *   From the Arab elite and community seats, this reading experiences as legitimate enforcement of a standing legal entitlement against encroachment — closer to a rope from their vantage. From the Zionist organizational seat, the identical clauses experience as an extraction mechanism actively blocking a legally sanctioned national project — closer to a snare from their vantage. British administrators occupy the seat where the tangled character is most visible: they administer a structure that is simultaneously coordination (of two conflicting international obligations) and extraction (of concessions from whichever party the moment's enforcement disfavors), which is why their situation is authored as active enforcement under cross-pressure rather than simple agenda-setting.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Arab communities and elites are the structural beneficiaries under this reading — the protective clauses exist, on this reading, to preserve their pre-existing status, so they sit near the beneficiary end of directionality despite lacking formal administrative power (their exit options are constrained because appeal runs through the same British/League channels contested by the rival reading). Zionist organizations and British administrators are payers: Zionist organizations because land and immigration restrictions directly cap their program, and British administrators because they bear the enforcement cost and cross-pressure of a duty this reading treats as binding but which conflicts with their other textual commitment. Rural tenant cultivators are excluded rather than directly represented, which the six_questions absent_voices field flags — they are the population nominally protected but do not control the reading's advocacy apparatus.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling a prior political promise with a new international legal duty to an existing majority population — remains genuinely contested through 1939 rather than resolved or abandoned, which is why founding_problem_status is 'contested' rather than 'dead': both the protective-obligation reading and the national-home-primacy reading continued to claim the same textual ground until the Mandate's termination. This prevents the tangled_rope classification from being mistaken as either pure coordination (it is not neutral; it structurally disadvantages Zionist demographic goals) or pure extraction (the protective function is not cover — the land tenure and political status it defends are real pre-existing entitlements, not manufactured grievances), which is exactly the ambiguity the tangled_rope category exists to hold without resolving in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_priority_ambiguity,
    'Does the Mandate for Palestine''s text actually establish the protective clauses as equal or superior to the national home clause, or is this reading importing a priority the 1922 instrument''s drafters did not intend?',
    'Close textual and drafting-history analysis of the Mandate document alongside League of Nations Council debate records and the Churchill White Paper (1922), comparing draftsmen''s stated intent against the plain-text ordering of clauses.',
    'If the drafting history shows the protective language was intended as subordinate qualification rather than coordinate obligation, this reading''s claimed legal basis weakens substantially and the constraint may better be understood as an interpretive overlay rather than a textually grounded obligation — shifting weight toward the mandatory_interpretive_discretion sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_priority_ambiguity, conceptual, 'Whether the dual-obligation reading is textually grounded or an interpretive construction layered onto ambiguous mandate language.').

omega_variable(
    arab_elite_representativeness,
    'How faithfully did the Arab Executive and notable-family petitioners represent the interests of rural tenant cultivators whose land tenure was the reading''s stated concern?',
    'Comparative analysis of land registry and eviction records against Arab Executive petition content and outcomes, assessing whether elite advocacy tracked or diverged from cultivator-level displacement patterns.',
    'A significant divergence would indicate the beneficiary group as authored (''palestinian_arab_communities'') should be split into elite and subaltern strata with different directionality, since the protective reading may have served landholding elite interests more directly than tenant interests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(arab_elite_representativeness, empirical, 'Whether elite political representation matched the material interests of the population the reading claims to protect.').

omega_variable(
    reading_incommensurability,
    'Given that this reading, jewish_national_home_primacy, and mandatory_interpretive_discretion all cite the same 1922 Mandate text, is the underlying document genuinely indeterminate between them, or did contemporaneous actors (British Cabinet, League Council) hold a settled private understanding that one reading was primary and the ambiguity is a later historiographical artifact?',
    'Archival review of British Cabinet and Colonial Office internal correspondence contemporaneous with mandate drafting (1920-22), assessing whether officials privately treated one reading as controlling regardless of public ambiguity.',
    'If a settled private understanding existed, the ''dual obligation'' as a live constraint may be better characterized as post-hoc advocacy rather than a genuine feature of the mandate''s operative design during the period studied, though it would remain a real historical force in the political contest even if not the drafters'' original intent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_incommensurability, empirical, 'Whether the kernel''s textual ambiguity is genuine or a retrospective reconstruction obscuring settled original intent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 1920, 1939).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1920, 0.55).
narrative_ontology:measurement(balf_tr_t1923, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1923, 0.5).
narrative_ontology:measurement(balf_tr_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1929, 0.46).
narrative_ontology:measurement(balf_tr_t1933, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1933, 0.44).
narrative_ontology:measurement(balf_tr_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1936, 0.4).
narrative_ontology:measurement(balf_tr_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1939, 0.42).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1920, 0.35).
narrative_ontology:measurement(balf_be_t1923, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1923, 0.42).
narrative_ontology:measurement(balf_be_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1929, 0.58).
narrative_ontology:measurement(balf_be_t1933, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1933, 0.64).
narrative_ontology:measurement(balf_be_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1936, 0.69).
narrative_ontology:measurement(balf_be_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1939, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1920, 0.35).
narrative_ontology:measurement(balf_su_t1923, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1923, 0.4).
narrative_ontology:measurement(balf_su_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1929, 0.55).
narrative_ontology:measurement(balf_su_t1933, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1933, 0.6).
narrative_ontology:measurement(balf_su_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1936, 0.72).
narrative_ontology:measurement(balf_su_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1939, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.12).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% This story is one of three constraints sharing the balfour_mandate_instruments kernel (the 1922 Mandate for Palestine text and its Covenant Article 22 backing). jewish_national_home_primacy inverts the beneficiary/victim structure entirely (Zionist organizations as beneficiary, Arab communities as target) reading the same clauses as directing demographic transformation. mandatory_interpretive_discretion locates the operative constraint in British adjudicative authority itself rather than in either substantive obligation, making the British administrator the agenda-setter and both Arab and Zionist parties into payers of a discretion tax. All three stories must be read together to understand the historical contest; none alone describes 'the Mandate' as a single settled constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
