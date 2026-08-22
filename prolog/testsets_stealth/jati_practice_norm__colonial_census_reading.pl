% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__colonial_census_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__colonial_census_reading, []).

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
 *   constraint_id: jati_practice_norm__colonial_census_reading
 *   human_readable: Colonial Census Reification of Jati Categories
 *   domain: social_anthropology/political_economy
 *
 * SUMMARY:
 *   Beginning with the 1871 all-India census and maturing through the
 *   ethnographic surveys, imperial gazetteers, recruitment tables, and
 *   scheduled-lists orders of the following seventy-five years, an external
 *   administrative apparatus converted jati, a proliferating and locally
 *   negotiated field of kin-and-occupation statuses, into a fixed grid of
 *   enumerable, rankable, administrable categories. This file instantiates
 *   the colonial_census_reading of the jati_practice_norm kernel: the
 *   standing arrangement under assessment is the stabilized category system
 *   itself, and epsilon is authored for that arrangement as this reading sees
 *   it, namely externally imposed fixity that benefited administrative
 *   legibility at the cost of community autonomy over self-definition. The
 *   sibling readings are separate constraints with their own epsilon values,
 *   victim sets, and types; they are linked through the network, not averaged
 *   here. KEY AGENTS (by structural relationship):
 *   colonial_administrative_apparatus: founding agenda-setter
 *   (institutional/arbitrage), built and enforced the grid, captured its
 *   legibility rents, then exited entirely at decolonization;
 *   state_classification_apparatus: successor agenda-setter and beneficiary
 *   (institutional/constrained), administers the inherited lists;
 *   orientalist_ethnographic_establishment: beneficiary (organized/mobile),
 *   converted enumeration into scholarly authority;
 *   enumerated_jati_communities: primary payer (powerless/identity_locked),
 *   bear the frozen identities as constitutive; denotified_nomadic_tribes:
 *   extreme payer (powerless/trapped), hereditary-criminality designation and
 *   its afterlife; intermediate_status_claimant_castes: payer with partial
 *   beneficiary position (organized/constrained);
 *   reservation_eligible_communities: beneficiary and payer
 *   (organized/identity_locked); anti_caste_radical_traditions: excluded
 *   (organized/constrained); superior_courts: observer
 *   (institutional/analytical); subaltern_studies_historians: analytical
 *   observer (organized/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, 0.65).
domain_priors:suppression_score(jati_practice_norm__colonial_census_reading, 0.4).
domain_priors:theater_ratio(jati_practice_norm__colonial_census_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__colonial_census_reading, tangled_rope).
narrative_ontology:human_readable(jati_practice_norm__colonial_census_reading, "Colonial Census Reification of Jati Categories").
narrative_ontology:topic_domain(jati_practice_norm__colonial_census_reading, "social_anthropology/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__colonial_census_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, '75e15362-d85c-420b-a920-cbc1797bc7ec').
narrative_ontology:cs_kernel_codification('75e15362-d85c-420b-a920-cbc1797bc7ec', formalized).
narrative_ontology:cs_authority_grounding('75e15362-d85c-420b-a920-cbc1797bc7ec', extraction).
narrative_ontology:cs_interpretation_layer_present('75e15362-d85c-420b-a920-cbc1797bc7ec').
narrative_ontology:cs_reading_relation('75e15362-d85c-420b-a920-cbc1797bc7ec', jati_practice_norm__orthodox_textual_reading, influences).
narrative_ontology:cs_reading_relation('75e15362-d85c-420b-a920-cbc1797bc7ec', jati_practice_norm__localized_practice_reading, coexists_with).
narrative_ontology:cs_axiom('75e15362-d85c-420b-a920-cbc1797bc7ec', foundational, category_fixation_is_administratively_produced).
narrative_ontology:cs_axiom_status(category_fixation_is_administratively_produced, holdable).
narrative_ontology:cs_axiom_grounding('75e15362-d85c-420b-a920-cbc1797bc7ec', category_fixation_is_administratively_produced, empirically_contingent).
narrative_ontology:cs_axiom('75e15362-d85c-420b-a920-cbc1797bc7ec', secondary, legibility_requires_fixed_categories).
narrative_ontology:cs_axiom_status(legibility_requires_fixed_categories, holdable).
narrative_ontology:cs_axiom_grounding('75e15362-d85c-420b-a920-cbc1797bc7ec', legibility_requires_fixed_categories, instrumental).
narrative_ontology:cs_reference_frame('75e15362-d85c-420b-a920-cbc1797bc7ec', census_grid_legible_order).
narrative_ontology:cs_drift_state('75e15362-d85c-420b-a920-cbc1797bc7ec', postcolonial_contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('75e15362-d85c-420b-a920-cbc1797bc7ec', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_administrative_apparatus).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, orientalist_ethnographic_establishment).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, state_classification_apparatus).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, enumerated_jati_communities).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, denotified_nomadic_tribes).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, intermediate_status_claimant_castes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, intermediate_status_claimant_castes).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, reservation_eligible_communities).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, reservation_eligible_communities).
narrative_ontology:constraint_vindicates(jati_practice_norm__colonial_census_reading, martial_race_theory).
narrative_ontology:constraint_vindicates(jati_practice_norm__colonial_census_reading, ethnographic_state_legibility_doctrine).
narrative_ontology:constraint_vindicates(jati_practice_norm__colonial_census_reading, varna_as_observable_social_taxonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conducted the decennial censuses from 1871, compiled the gazetteers and ethnographic glossaries, ranked castes for military recruitment and land-revenue assessment, and drew the electorate schedules that allocated representation by category. It collected what the grid made possible: taxable, recruitable, countable populations. In 1947 it dissolved outright, taking its warrant with it.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, continental).

% Census commissioners, gazetteer authors, and their salaried pandit informants turned enumeration into ethnographic authority: published rankings, racial typologies, and consulting careers. When the empire ended, the personnel moved on to universities and other archives.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, orientalist_ethnographic_establishment, beneficiary,
    organized, biographical, mobile, continental).

% The successor bureaucracy, registrar-general offices, backward-classes commissions, and district certificate-issuing machinery, inherited the lists and now sets the procedures by which communities enter, leave, or split categories. Budgets, staffing, and adjudicative power attach to administering classifications it did not create and cannot cheaply abandon, because the welfare architecture hangs on them.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, state_classification_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, state_classification_apparatus, beneficiary).

% Answered the enumerators and have carried the recorded labels ever since: marriage arrangements, occupational niches, and political voice route through the category the ledger fixed. Changing the recorded identity is procedurally arduous and socially self-denying, because the label is held jointly by the state record and the community's own practice.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, enumerated_jati_communities, payer,
    powerless, generational, identity_locked, national).

% Were registered under the 1871 legislation as hereditarily criminal, subject to passes, movement restrictions, and settlement camps. De-notification in 1952 removed the statute but not the registry habits, police suspicion, or the stigma that follows the label. They have the least procedural purchase on their classification of any group in the system.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, denotified_nomadic_tribes, payer,
    powerless, generational, trapped, national).

% Large agrarian communities whose standing in the official hierarchy sits uncomfortably with their local power. They mount mass agitation, road blockades and quota protests, to move between official categories as the material value of each shifts, petitioning commissions they do not control. They denounce the rigidity of the lists while campaigning for firmer placement inside them.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, intermediate_status_claimant_castes, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, intermediate_status_claimant_castes, beneficiary).

% Receive educational seats, public employment, and legislative reservations keyed to listed status. Defending the listing defends the benefit stream, so these communities often police their own category boundaries more fiercely than the bureaucracy does, while still bearing the frozen-identity costs and competing within the quota among themselves.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, reservation_eligible_communities, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, reservation_eligible_communities, payer).

% From the nineteenth-century reformers through the annihilation-of-caste line to contemporary rationalist movements, they reject the classificatory frame itself rather than any placement within it. They appear in commission proceedings only as witnesses; their remedy, no categories at all, has no filing door.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, anti_caste_radical_traditions, excluded,
    organized, civilizational, constrained, national).

% Adjudicate the disputes the lists generate: reservation ceilings, sub-categorization, income exclusions. They treat the schedules as facts to be interpreted and applied, examining procedure and evidence within the frame rather than the frame itself.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, superior_courts, observer,
    institutional, generational, analytical, national).

% Archival scholarship reconstructing pre-census boundary fluidity and documenting the census's constructive role. They bear none of the constraint's costs and collect none of its gains; their exit is the archive.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, subaltern_studies_historians, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__colonial_census_reading, state_classification_apparatus).
narrative_ontology:fixing_cost_class(jati_practice_norm__colonial_census_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Produced a single standardized, comparable inventory of a vast heterogeneous population: fixed labels let distant offices assess land revenue, recruit regiments, allocate electorates, and target famine and epidemic relief without renegotiating each locality's self-descriptions.
% TRANSFER_FUNCTION: Moves authority over social self-definition from jati communities to the central classificatory apparatus: communities supply answers, the apparatus fixes the categories and ranks them, and legal consequences, tax liability, recruitment eligibility, electorate membership, later reservation entitlement, attach to labels communities cannot unilaterally revise.
% ABSENT_VOICES: At enumeration the counted were not consulted: women's status was recorded via male household heads, illiterate respondents answered through intermediaries, and dissenting sects that refused categorization were entered under officer-assigned labels. Anti-caste radicals who rejected the classificatory frame entirely had no procedural seat; they appear in the record only as objects classified.
% DISAPPEARANCE_RATIONALE: Constitutional reservation quotas, backward-class commissions, caste-based electoral mobilization, and marriage-market filtering all run on the frozen lists; overnight disappearance would strand the welfare architecture without an operational basis, force immediate renegotiation of thousands of status claims, and dissolve the constituency maps parties campaign on.
% FOUNDING_PROBLEM: Governing a subcontinent through offices that could not see it: revenue assessment, military recruitment, policing, and eventually controlled representation required a standardized picture of who lived where, under what name, in what number.
% FOUNDING_PROBLEM_CORROBORATION: No contemporary institution defends colonial legibility as the operative justification: the successor apparatus cites welfare targeting and discrimination remediation instead, and the founding problem's owner dissolved in 1947. Outside the benefiting parties, decolonization itself removed the problem's bearer, and the historiography of the census, from Cohn and Dirks through the subaltern-studies literature, documents the founding rationale as an artifact of colonial governance rather than a live necessity.
narrative_ontology:disappearance_verdict(jati_practice_norm__colonial_census_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__colonial_census_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__colonial_census_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jati_practice_norm__colonial_census_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__colonial_census_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__colonial_census_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__colonial_census_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.65: the freeze expropriated authority over self-definition and attached legal consequences to the labels, but a real coordination function, a single comparable inventory of a vast heterogeneous population, runs through the same structure, which caps any pure-extraction reading. Suppression 0.40 is deliberately authored above the final suppression_requirement series value (0.22) because the scalar captures the constraint's total structural suppressive force, certificate regimes, petition burdens, list rigidity, plus internalized components, while the series tracks active enforcement capacity alone, which decayed sharply after 1947; the widening gap between the scalar and the series tail is the finding, not an inconsistency. Theater 0.55: post-colonial maintenance is increasingly ritual, decennial listing exercises and quota arithmetic performed over a legibility function whose original consumer is gone. Accessibility_collapse 0.60: once categories carried material stakes, the alternative of fluid local renegotiation largely collapsed, communities now defend their own listings, but petition doors (reclassification, sub-categorization) keep collapse short of total. Resistance 0.55: sustained across the interval, from non-Brahmin movements and census-era misreporting to the modern agitation waves that are themselves resistance to rigidity. All three series share one time grid (0/25/50/75/100/125/150). Claim and metrics are independent: claimed_type tangled_rope is the structural truth this reading asserts; the rising theater tail stands as drift signal rather than being reconciled away.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the colonial agenda-setter seat the grid was instrument-building, a tool it wielded and then abandoned; from the enumerated communities' seat the same grid operates as constitutive capture, and identity_locked targets sit near the full-target end of directionality, so effective extraction amplifies for them specifically. The successor apparatus experiences the lists as inherited facticity: its institutional power depends on not reopening the question of where the categories came from. The identity-lock mechanism is institutional-relational fusion: the state record and community practice (marriage, occupation, political voice) fused around the recorded label, so exit would require disowning an identity both the ledger and the community hold; if that frame broke, through a records amnesty plus detached material stakes, local renegotiation would resume, which is precisely the localized reading's counterfactual. Coalition note: the payer seats are not helpless, cross-community agitation coalitions and the demographic weight of listed categories in elections constitute real coalition power that has periodically forced the agenda-setter seat to concede reclassification.
 *
 * DIRECTIONALITY LOGIC:
 *   The three declared beneficiary seats derive low directionality, subsidy side: the colonial apparatus collected legibility directly, the ethnographic establishment converted it into careers and epistemic authority, and the successor apparatus accrues institutional power from gatekeeping. The declared victim seats derive high directionality, amplified for identity_locked enumerated communities and maximal for trapped denotified tribes. Two dual-positioned agents are handled structurally rather than by override: reservation_eligible_communities carry beneficiary-primary with payer-secondary, so their material receipts damp directionality while their autonomy loss raises it, netting near symmetric; intermediate_status_claimant_castes carry payer-primary with beneficiary-secondary, bearing rigidity while seeking firmer placement inside the grid. The derivation chain resolves both from the declared roles and exit options, so no explicit directionality overrides are used.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, colonial governance legibility, is dead: its owner dissolved in 1947 and no contemporary institution defends legibility as the operative justification, the successor apparatus cites welfare targeting and discrimination remediation instead. The R5 mismatch (status dead combined with verdict world_rearranges) should fire the capture/zombie flag, and the theater series crossing 0.5 supplies the corroborating drift signal. The classification guards against two opposite mislabels: reading the arrangement as pure extraction ignores the genuine coordination function that still runs through it, standardized comparable categories on which the welfare architecture depends; reading it as pure coordination ignores the asymmetric, actively enforced transfer of self-definition authority away from communities. The post-colonial segment trends toward inertia, enforcement decayed while extraction persisted and theater rose, and the temporal series is authored so the engine can date any such transition rather than the claim asserting it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the stabilized-category arrangement this story assesses genuinely produced by external administrative freezing (this reading), or is the same arrangement better described by a sibling reading of the jati_practice_norm kernel?',
    'Comparative archival work correlating pre-census local boundary practice (mobility, proliferation, renaming) with census-era fixation rates; adoption of a sibling reading routes assessment to that reading''s own constraint file.',
    'Under localized_practice_reading the freeze is one episode in continuous renegotiation and epsilon falls toward coordination-cost levels; under orthodox_textual_reading the victim structure shifts to pollution-enforcement of ritual-status deviants. Either shift changes type and per-seat classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the jati kernel the assessed arrangement belongs to.').

omega_variable(
    counterfactual_indigenous_hardening,
    'Would jati categories have hardened substantially without the external apparatus, through Sanskritization dynamics, print vernacularization, and the enumerations conducted by native states such as Mysore, Baroda, and Travancore?',
    'Compare boundary-fixity trajectories in regions under direct British census administration against princely-state and non-enumerated regions with similar commercialization and print exposure.',
    'If hardening proceeds comparably without the apparatus, the external-freezing causal claim weakens, the epsilon attributable to this constraint drops, and the reading converges toward the localized reading''s account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_indigenous_hardening, empirical, 'Tests the reading''s core causal attribution of reification to the external apparatus.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression that maintains category adherence today structural (certificate regimes, petition burdens, list rigidity) or internalized (communities experiencing census identities as primordial)?',
    'Track classification-petition volumes and identity-salience shifts where material stakes detach from listings, such as urban anonymized labor markets; persistent adherence after barrier removal indicates internalization.',
    'If largely internalized, suppression outlives enforcement decay, consistent with the falling suppression_requirement series running alongside persistent extraction, and exit assessments soften upward for younger cohorts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of residual suppression after enforcement decay.').

omega_variable(
    quota_community_positional_weight,
    'How much beneficiary weight do reservation-receiving communities carry against their payer weight: does material receipt through the frozen lists offset autonomy loss in the directionality derivation?',
    'Welfare-transfer incidence analysis against measured demand for delisting or reclassification; revealed preference where communities decline beneficial listings.',
    'High beneficiary weight damps aggregate effective extraction and strengthens the coordination half of the hybrid verdict; low weight leaves the extraction asymmetry dominant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quota_community_positional_weight, preference, 'Positional weighting of dual-positioned quota communities.').

omega_variable(
    orthodox_fusion_extent,
    'To what extent did the census apparatus merely operationalize pre-existing scriptural authority, as its use of salaried pandit informants and shastric ranking suggests, rather than independently producing fixation?',
    'Trace the provenance of specific census rankings to informant networks and textual citations; quantify how many official placements reproduce prior textual schemes versus novel administrative judgments.',
    'High fusion relocates part of the extraction''s origin to the orthodox_textual_reading''s constraint and makes the influences edge effectively bidirectional; low fusion confirms administrative production as the distinguishing mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(orthodox_fusion_extent, empirical, 'Degree of fusion between census practice and scriptural authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__colonial_census_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_colonial_census_tr_t0, jati_practice_norm__colonial_census_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(jati_colonial_census_tr_t0, observed).
narrative_ontology:measurement(jati_colonial_census_tr_t25, jati_practice_norm__colonial_census_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(jati_colonial_census_tr_t25, observed).
narrative_ontology:measurement(jati_colonial_census_tr_t50, jati_practice_norm__colonial_census_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement_basis(jati_colonial_census_tr_t50, observed).
narrative_ontology:measurement(jati_colonial_census_tr_t75, jati_practice_norm__colonial_census_reading, theater_ratio, 75, 0.34).
narrative_ontology:measurement_basis(jati_colonial_census_tr_t75, observed).
narrative_ontology:measurement(jati_colonial_census_tr_t100, jati_practice_norm__colonial_census_reading, theater_ratio, 100, 0.44).
narrative_ontology:measurement_basis(jati_colonial_census_tr_t100, observed).
narrative_ontology:measurement(jati_colonial_census_tr_t125, jati_practice_norm__colonial_census_reading, theater_ratio, 125, 0.52).
narrative_ontology:measurement_basis(jati_colonial_census_tr_t125, observed).
narrative_ontology:measurement(jati_colonial_census_tr_t150, jati_practice_norm__colonial_census_reading, theater_ratio, 150, 0.55).
narrative_ontology:measurement_basis(jati_colonial_census_tr_t150, observed).

% Extraction over time
narrative_ontology:measurement(jati_colonial_census_be_t0, jati_practice_norm__colonial_census_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(jati_colonial_census_be_t0, observed).
narrative_ontology:measurement(jati_colonial_census_be_t25, jati_practice_norm__colonial_census_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement_basis(jati_colonial_census_be_t25, observed).
narrative_ontology:measurement(jati_colonial_census_be_t50, jati_practice_norm__colonial_census_reading, base_extractiveness, 50, 0.66).
narrative_ontology:measurement_basis(jati_colonial_census_be_t50, observed).
narrative_ontology:measurement(jati_colonial_census_be_t75, jati_practice_norm__colonial_census_reading, base_extractiveness, 75, 0.71).
narrative_ontology:measurement_basis(jati_colonial_census_be_t75, observed).
narrative_ontology:measurement(jati_colonial_census_be_t100, jati_practice_norm__colonial_census_reading, base_extractiveness, 100, 0.69).
narrative_ontology:measurement_basis(jati_colonial_census_be_t100, observed).
narrative_ontology:measurement(jati_colonial_census_be_t125, jati_practice_norm__colonial_census_reading, base_extractiveness, 125, 0.67).
narrative_ontology:measurement_basis(jati_colonial_census_be_t125, observed).
narrative_ontology:measurement(jati_colonial_census_be_t150, jati_practice_norm__colonial_census_reading, base_extractiveness, 150, 0.65).
narrative_ontology:measurement_basis(jati_colonial_census_be_t150, observed).

% Suppression requirement over time
narrative_ontology:measurement(jati_colonial_census_su_t0, jati_practice_norm__colonial_census_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(jati_colonial_census_su_t0, observed).
narrative_ontology:measurement(jati_colonial_census_su_t25, jati_practice_norm__colonial_census_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(jati_colonial_census_su_t25, observed).
narrative_ontology:measurement(jati_colonial_census_su_t50, jati_practice_norm__colonial_census_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement_basis(jati_colonial_census_su_t50, observed).
narrative_ontology:measurement(jati_colonial_census_su_t75, jati_practice_norm__colonial_census_reading, suppression_requirement, 75, 0.6).
narrative_ontology:measurement_basis(jati_colonial_census_su_t75, observed).
narrative_ontology:measurement(jati_colonial_census_su_t100, jati_practice_norm__colonial_census_reading, suppression_requirement, 100, 0.3).
narrative_ontology:measurement_basis(jati_colonial_census_su_t100, observed).
narrative_ontology:measurement(jati_colonial_census_su_t125, jati_practice_norm__colonial_census_reading, suppression_requirement, 125, 0.25).
narrative_ontology:measurement_basis(jati_colonial_census_su_t125, observed).
narrative_ontology:measurement(jati_colonial_census_su_t150, jati_practice_norm__colonial_census_reading, suppression_requirement, 150, 0.22).
narrative_ontology:measurement_basis(jati_colonial_census_su_t150, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__colonial_census_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, localized_practice_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (jati_practice_norm), three epsilon-invariant readings. This file authors epsilon 0.65 for the externally stabilized category arrangement. orthodox_textual_reading authors its own epsilon for a scripture-grounded pollution-enforcement structure with a different victim set (ritual-status deviants); localized_practice_reading authors low epsilon for continuously renegotiated local coordination norms. Upstream/downstream: the census reading historically operationalized the orthodox scheme through state-commissioned rankings and paid informants, hence the influences edge, while subordinating local practice to the official grid. Each sibling links back; no averaging across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
