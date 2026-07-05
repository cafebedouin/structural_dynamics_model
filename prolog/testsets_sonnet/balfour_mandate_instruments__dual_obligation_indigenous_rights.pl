% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__dual_obligation_indigenous_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Mandate Dual-Obligation Reading: Indigenous Rights Subordinate the National Home
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This story instantiates one contested reading of the British Mandate for
 *   Palestine's founding text: that the instrument's Article 2 protections
 *   for 'civil and religious rights' and its grounding in League of Nations
 *   self-determination and minority-protection norms impose an obligation to
 *   protect existing Arab civil, political, and land-tenure interests that is
 *   equal to, or supersedes, the national-home clause. Under this reading,
 *   land transfer restrictions (culminating in measures like the 1930 Hope
 *   Simpson report's findings and the 1940 Land Transfer Regulations, near
 *   the edge of this interval) and immigration ceilings tied to 'economic
 *   absorptive capacity' are read as the mandate performing its actual
 *   textual obligation rather than as concessions to Arab pressure. The
 *   sibling readings — that the instrument primarily mandates demographic
 *   transformation toward Jewish sovereignty, and that British interpretive
 *   discretion itself is the operative constraint — are separate constraints
 *   with their own epsilon and are not blended into this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.71).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.62).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.71).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Mandate Dual-Obligation Reading: Indigenous Rights Subordinate the National Home").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'b0f73dde-524b-46ba-8098-6edcfd46af4b').
narrative_ontology:cs_kernel_codification('b0f73dde-524b-46ba-8098-6edcfd46af4b', fixed_text).
narrative_ontology:cs_authority_grounding('b0f73dde-524b-46ba-8098-6edcfd46af4b', extraction).
narrative_ontology:cs_interpretation_layer_present('b0f73dde-524b-46ba-8098-6edcfd46af4b').
narrative_ontology:cs_reading_relation('b0f73dde-524b-46ba-8098-6edcfd46af4b', balfour_mandate_instruments__jewish_national_home_primacy, coexists_with).
narrative_ontology:cs_reading_relation('b0f73dde-524b-46ba-8098-6edcfd46af4b', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('b0f73dde-524b-46ba-8098-6edcfd46af4b', foundational, existing_population_rights_precede_settler_facilitation).
narrative_ontology:cs_axiom_status(existing_population_rights_precede_settler_facilitation, holdable).
narrative_ontology:cs_axiom_grounding('b0f73dde-524b-46ba-8098-6edcfd46af4b', existing_population_rights_precede_settler_facilitation, deontological).
narrative_ontology:cs_axiom('b0f73dde-524b-46ba-8098-6edcfd46af4b', foundational, demographic_majority_grounds_sovereignty_claim).
narrative_ontology:cs_axiom_status(demographic_majority_grounds_sovereignty_claim, holdable).
narrative_ontology:cs_axiom_grounding('b0f73dde-524b-46ba-8098-6edcfd46af4b', demographic_majority_grounds_sovereignty_claim, conventional).
narrative_ontology:cs_reference_frame('b0f73dde-524b-46ba-8098-6edcfd46af4b', league_covenant_self_determination_standard).
narrative_ontology:cs_drift_state('b0f73dde-524b-46ba-8098-6edcfd46af4b', post_hope_simpson_1930, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b0f73dde-524b-46ba-8098-6edcfd46af4b', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, self_determination_norm_primacy).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, minority_protection_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Notable families, municipal councils, and the Arab Executive read Article 2's civil-and-religious-rights guarantee and the self-determination language of the Covenant as grounding a claim to eventual representative government and sovereignty proportional to demographic majority. They press the mandatory power through petitions, boycotts of legislative councils they see as insufficiently representative, and appeals to the Permanent Mandates Commission, using the instrument's own text as leverage against land transfer and immigration.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_elites, beneficiary,
    organized, generational, constrained, regional).

% Fellahin and smallholder villages whose tenure the land transfer restrictions are meant to protect. Where the protective provisions are enforced, existing cultivators retain access to land; where they are administratively loosened or unevenly applied, families are displaced despite the mandate's stated obligation. They have no direct standing before the Mandates Commission and depend entirely on the elite leadership and the administration acting on the text.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities, beneficiary,
    moderate, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_communities, payer).

% The Jewish Agency and affiliated bodies read the same mandate as authorizing large-scale land purchase and immigration to build the national home, and experience the dual-obligation reading as a structural cap: land transfer regulations restrict the land market they depend on, and immigration ceilings tied to 'absorptive capacity' and demographic-balance concerns limit the population growth their project requires. They cannot exit the mandate framework — their entire claim to Palestine runs through it — so they lobby London, fund settlement institutions, and contest administrative rulings, but cannot bypass the instrument itself.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_organizations, payer,
    organized, generational, constrained, global).

% The High Commissioner and Colonial Office staff must administer two textually co-equal or superior obligations — protecting existing Arab rights and tenure, and facilitating the national home — that pull toward opposite policy outcomes. Under this reading they are structurally bound to satisfy the indigenous-rights and self-determination language first, which means refusing or slow-walking Zionist demands they might otherwise wish to grant, and absorbing the resulting pressure from London, from Zionist lobbying, and from Arab unrest when enforcement is seen as inadequate.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administrators, payer,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administrators, agenda_setter).

% The League of Nations body reviews annual reports and petitions, adjudicating (without binding enforcement power) whether the mandatory power is honoring the minority-protection and self-determination language against the national-home clause. Its findings shape international legitimacy but cannot compel British compliance.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, permanent_mandates_commission, observer,
    institutional, generational, analytical, global).

% Tenant farmers and laborers actually displaced by land sales that occur despite the protective provisions — whether through absentee-landlord transfers, administrative loopholes, or uneven enforcement — have no seat at the Mandates Commission, no representation in petitions drafted by urban notables, and no exit from a rural economy being reshaped around them.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, arab_agricultural_laborers_and_tenants_displaced, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the mandate instrument coordinates the mandatory power's exercise of authority around a hierarchy of obligations: protecting the existing population's civil, political, and property rights takes precedence over (or stands equal to) facilitating Jewish immigration and settlement, so administrative discretion is bounded rather than open-ended.
% TRANSFER_FUNCTION: The reading transfers policy leverage and land-market access from Zionist organizations toward Arab communities and their political leadership: land transfer restrictions keep tenure in existing hands, and immigration ceilings keep political and demographic weight from shifting away from the Arab majority.
% ABSENT_VOICES: Displaced tenant farmers and landless laborers are structurally absent from both the Mandates Commission process and the elite-led petition politics that invoke the dual-obligation language on their behalf; their concrete tenure losses are not what the elite claims to the Commission actually track.
% DISAPPEARANCE_RATIONALE: If this reading's obligations were not asserted or enforced at all, land transfer would proceed largely unrestricted, immigration would scale to Zionist organizational capacity rather than administrative ceilings, and the demographic and political trajectory toward a Jewish-majority polity would accelerate substantially faster than it did — the entire subsequent contest over partition and statehood assumes this reading's obligations were at least partially binding on British policy.
% FOUNDING_PROBLEM: The mandate system was built to reconcile League of Nations self-determination and minority-protection principles (adopted post-WWI as the normative basis for administering former Ottoman territories) with Britain's prior Balfour Declaration commitment to a Jewish national home, in a territory whose population was overwhelmingly Arab at the time of drafting.
% FOUNDING_PROBLEM_CORROBORATION: Arab delegations and the King-Crane Commission (a body external to both the Zionist movement and the British administration, commissioned by the US in 1919) attested that the indigenous population's self-determination claim was the founding problem and remained live and unresolved; the Permanent Mandates Commission's own annual review debates repeatedly registered unresolved tension between the two obligations rather than treating the indigenous-rights obligation as settled or extinguished.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__dual_obligation_indigenous_rights, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__dual_obligation_indigenous_rights, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.71 by 1939) because under this reading the mandate structure actively transfers land-market and demographic leverage away from Zionist organizations toward Arab communities and their leadership, and this transfer intensifies as land regulations and immigration ceilings harden through the 1930s. Suppression (0.62) reflects the active administrative and coercive apparatus required to hold the restrictions in place against sustained Zionist lobbying and organizational pressure. Theater ratio (0.4) captures a real but only partially enforced protective function: elite Arab leadership's engagement with the Mandates Commission produces real policy constraint but also a documentary record that outruns what happens to actual tenant farmers on the ground. Accessibility collapse is moderate (0.5) — the mandate framework itself is inescapable for all major parties, but alternative political arrangements (partition, independence, unrestricted Zionist settlement) remain live and contested throughout, so alternatives have not collapsed as completely as under a mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian Arab elites and communities are structural beneficiaries under this reading: the instrument's protective language is read as running in their favor, grounding both tenure security and a path to representative government proportional to demographic weight — low d. Zionist organizations and British administrators sit at the target end: Zionist organizations are structurally blocked from the land access and demographic parity their project requires, and cannot exit the framework since their claim to Palestine is constituted by it (constrained exit, high d); British administrators are institutionally bound to enforce obligations that put them in permanent conflict with a commitment (Balfour) they are simultaneously charged with honoring, which extracts from their administrative capacity and political standing even though they are also the agenda-setter administering the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The dual-obligation reading prevents the mandate from being read as pure cover for demographic engineering (which would make it a pure snare against Arab communities) by identifying a genuine, textually grounded coordination function — the League's self-determination and minority-protection commitments were real normative constraints on interwar mandatory power, not manufactured after the fact. But it also prevents treating the mandate as pure benign coordination, because enforcing this reading required active, intensifying suppression against a well-organized rival claimant (Zionist organizations) and against the mandatory power's own competing commitment — hence tangled_rope rather than rope: real coordination function, real asymmetric extraction, both riding the same instrument.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_hierarchy_indeterminacy,
    'Does the mandate instrument''s text actually establish the indigenous-rights obligation as superior to, equal to, or subordinate to the national-home clause, or is the hierarchy itself underdetermined by the drafting history?',
    'Close comparative reading of the mandate''s preamble and Articles 2, 6, and 22 against the drafting correspondence between the Foreign Office, the Zionist Organization, and Arab delegations in 1920-1922, cross-checked against contemporaneous Permanent Mandates Commission interpretive practice.',
    'If the drafting history shows the indigenous-rights language was intended as a subordinate qualifier rather than a co-equal or superior obligation, this reading''s claimed textual grounding weakens substantially and the constraint''s classification may shift toward a weaker coordination function riding mostly on Arab political mobilization rather than instrument text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_hierarchy_indeterminacy, conceptual, 'Whether the mandate text itself supports a hierarchy favoring indigenous rights, or whether that hierarchy is a later interpretive overlay.').

omega_variable(
    elite_versus_mass_beneficiary_divergence,
    'Does the protective function primarily benefit the Arab political and landowning elite who engage the Mandates Commission process, or does it reach the tenant farmers and laborers whose land tenure is nominally protected?',
    'Land registry and displacement records for the mandate period, cross-referenced against which land transfers were actually blocked versus which proceeded despite the regulations, disaggregated by class.',
    'If protection is substantially captured by elite intermediaries while mass displacement continues, the beneficiary declaration should be narrowed or split into a separate story distinguishing elite from mass beneficiaries, since their structural positions and actual receipt of protection diverge sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_versus_mass_beneficiary_divergence, empirical, 'Whether the protective obligation''s benefits reach the population it names or are captured by elite intermediaries.').

omega_variable(
    reading_framing_underdetermination,
    'Is the choice to treat this as the dominant or a co-equal reading of the kernel itself contestable, given that British administrative practice through the 1920s often acted more consistently with the jewish_national_home_primacy reading than with this one?',
    'Track actual administrative outcomes (land transfer approvals, immigration certificate issuance rates) against which reading''s predictions they match more closely, period by period.',
    'If administrative practice tracks the primacy reading more closely for most of the interval and only shifts toward the dual-obligation reading after 1930 (Hope Simpson, Passfield White Paper), the constraint''s ε trajectory and the classification of ''this reading'' as descriptively operative (versus merely textually available) would need to be revisited, likely narrowing the interval over which it is treated as the operative constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Whether this reading was ever the operative administrative practice or remained primarily a textual/legal claim contested against actual British conduct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 1920, 1939).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1920, 0.3).
narrative_ontology:measurement(balf_tr_t1923, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1923, 0.32).
narrative_ontology:measurement(balf_tr_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1929, 0.36).
narrative_ontology:measurement(balf_tr_t1933, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1933, 0.38).
narrative_ontology:measurement(balf_tr_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1936, 0.4).
narrative_ontology:measurement(balf_tr_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1939, 0.4).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1920, 0.45).
narrative_ontology:measurement(balf_be_t1923, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1923, 0.5).
narrative_ontology:measurement(balf_be_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1929, 0.6).
narrative_ontology:measurement(balf_be_t1933, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1933, 0.65).
narrative_ontology:measurement(balf_be_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1936, 0.7).
narrative_ontology:measurement(balf_be_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1939, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1920, 0.4).
narrative_ontology:measurement(balf_su_t1923, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1923, 0.45).
narrative_ontology:measurement(balf_su_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1929, 0.55).
narrative_ontology:measurement(balf_su_t1933, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1933, 0.58).
narrative_ontology:measurement(balf_su_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1936, 0.62).
narrative_ontology:measurement(balf_su_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1939, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'the Balfour Mandate' per the ε-invariance principle: dual_obligation_indigenous_rights (this file, tangled_rope, high ε favoring Arab tenure/self-determination), jewish_national_home_primacy (sibling, ε favoring Zionist demographic/territorial claims), and mandatory_interpretive_discretion (sibling, locating the operative constraint in unreviewable British adjudicating authority rather than either substantive claim). Each carries its own stable ε and stakeholder structure; none averages or blends into the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
