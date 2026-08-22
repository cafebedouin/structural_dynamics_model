% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__national_liberation_reading, []).

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
 *   constraint_id: zionist_legitimacy_basis__national_liberation_reading
 *   human_readable: Zionism as National Liberation of a Persecuted Indigenous People (National Liberation Reading)
 *   domain: political_history/nationalism/settler_colonialism_studies
 *
 * SUMMARY:
 *   From 1897 (First Zionist Congress) forward, this reading holds that
 *   centuries of persecution culminating in the Holocaust, combined with
 *   continuous historical and religious connection to the land, establish a
 *   legitimate claim to national self-determination that overrides — or at
 *   minimum outweighs — the objections of the Arab population already
 *   resident there. The reading is genuinely responsive to a real
 *   coordination problem: Jewish statelessness had proven repeatedly and
 *   catastrophically dangerous, and no other state offered durable refuge.
 *   But the same reading's core premise — that persecution and ancestral
 *   connection justify displacement, and that resistance to that displacement
 *   is illegitimate denial of Jewish rights rather than a rival national
 *   claim — is also the mechanism by which the costs borne by Palestinian
 *   Arab residents and refugees are structurally excluded from the legitimacy
 *   calculus. The extraction is real (land, sovereignty, and residency
 *   transferred from one population to another) even though the coordination
 *   function (refuge for a persecuted people) is also real; that is why this
 *   reading computes as tangled_rope rather than as pure rope or pure snare
 *   from its own authored data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, 0.58).
domain_priors:suppression_score(zionist_legitimacy_basis__national_liberation_reading, 0.55).
domain_priors:theater_ratio(zionist_legitimacy_basis__national_liberation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__national_liberation_reading, "Zionism as National Liberation of a Persecuted Indigenous People (National Liberation Reading)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__national_liberation_reading, "political_history/nationalism/settler_colonialism_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__national_liberation_reading, '015f909c-eba2-4a87-bcac-2a0fe9a823ec').
narrative_ontology:cs_kernel_codification('015f909c-eba2-4a87-bcac-2a0fe9a823ec', distributed).
narrative_ontology:cs_authority_grounding('015f909c-eba2-4a87-bcac-2a0fe9a823ec', distributed).
narrative_ontology:cs_reading_relation('015f909c-eba2-4a87-bcac-2a0fe9a823ec', zionist_legitimacy_basis__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('015f909c-eba2-4a87-bcac-2a0fe9a823ec', zionist_legitimacy_basis__religious_restoration_reading, influences).
narrative_ontology:cs_axiom('015f909c-eba2-4a87-bcac-2a0fe9a823ec', foundational, persecution_and_ancestral_connection_ground_return_right).
narrative_ontology:cs_axiom_status(persecution_and_ancestral_connection_ground_return_right, holdable).
narrative_ontology:cs_axiom_grounding('015f909c-eba2-4a87-bcac-2a0fe9a823ec', persecution_and_ancestral_connection_ground_return_right, deontological).
narrative_ontology:cs_axiom('015f909c-eba2-4a87-bcac-2a0fe9a823ec', foundational, arab_opposition_constitutes_denial_of_jewish_national_rights).
narrative_ontology:cs_axiom_status(arab_opposition_constitutes_denial_of_jewish_national_rights, holdable).
narrative_ontology:cs_axiom_grounding('015f909c-eba2-4a87-bcac-2a0fe9a823ec', arab_opposition_constitutes_denial_of_jewish_national_rights, conventional).
narrative_ontology:cs_reference_frame('015f909c-eba2-4a87-bcac-2a0fe9a823ec', persecution_based_national_return_right).
narrative_ontology:cs_drift_state('015f909c-eba2-4a87-bcac-2a0fe9a823ec', post_1993_oslo_and_after, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('015f909c-eba2-4a87-bcac-2a0fe9a823ec', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, jewish_returnees_and_settlers).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, israeli_state_apparatus).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_arab_residents_1917_1948).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_refugees_and_descendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, international_jewish_diaspora_communities).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, jewish_returnees_and_settlers).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, jewish_indigeneity_to_the_land).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, national_self_determination_for_persecuted_peoples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Arrive fleeing pogroms, exclusion, and later genocide, understanding return to the ancestral homeland as the only durable answer to millennia of persecution and statelessness elsewhere. They organize settlement, land purchase, and eventually state institutions on the premise that historical and religious continuity with the land, combined with the absence of any other safe refuge, justifies establishing a Jewish national home even where it displaces existing residents. Many bear real costs — war, siege, loss of prior life elsewhere — but the reading treats the founding project itself as unambiguously redemptive rather than one input into a contested outcome.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, jewish_returnees_and_settlers, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__national_liberation_reading, jewish_returnees_and_settlers, payer).

% Administers immigration law (Law of Return), land policy, and military and diplomatic defense of the state founded on this reading. It sets the terms under which Jewish return is codified as right and Arab residence claims are processed as a security or demographic problem rather than a competing national claim of equal standing. It benefits from international recognition, alliance structures, and legal frameworks built on the national-liberation narrative, and has the institutional capacity to revise land, citizenship, and recognition policy but has not done so in ways that resolve the displaced population's claims.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Lived on and worked the land under Ottoman and then British Mandate rule as the demographic majority; experienced land purchase, immigration waves, and eventually war and mass displacement (1947-49) as the practical content of the 'return.' Under this reading their opposition to Jewish immigration and statehood is read as illegitimate resistance to a persecuted people's rights rather than as a rival national claim, which forecloses their objections from counting as a competing indigeneity claim on the same land.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_arab_residents_1917_1948, payer,
    powerless, biographical, trapped, national).

% Descendants of those displaced in 1948 and subsequent conflicts, dispersed across refugee camps, host states, and diaspora, largely without a right of return recognized by the state whose founding this reading legitimates. Their multigenerational claim is structurally excluded from the liberation narrative's accounting, since acknowledging it symmetrically would require treating Jewish return and Arab return as commensurable claims — which this reading's core premise does not permit.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_refugees_and_descendants, payer,
    powerless, civilizational, trapped, regional).

% Gain a guaranteed place of refuge and a locus of national identity and political solidarity, without necessarily bearing the costs of settlement or ongoing conflict themselves. Many actively fund, lobby for, and politically defend the state on the strength of the national-liberation account, and are shielded from most of the reading's practical costs by geographic distance.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, international_jewish_diaspora_communities, beneficiary,
    organized, generational, mobile, global).

% Absorbed successive waves of refugees, fought and lost wars framed around this reading's contested legitimacy, and are treated within the reading primarily as obstacles to Jewish self-determination rather than as parties with an independent account of regional history and displaced population obligations.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, regional_arab_states_and_publics, excluded,
    organized, generational, constrained, regional).

% Examine Ottoman land records, Mandate demographic data, League of Nations documents, and testimony from all parties to assess competing indigeneity and displacement claims. Their findings are cited selectively by all three kernel readings to support incompatible conclusions about who was indigenous, who was settler, and what obligations follow.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, historians_and_legal_scholars_of_the_mandate_period, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__national_liberation_reading, diffuse).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a persecuted diaspora people with a coordinated national project — collective self-defense, sovereign refuge, and institutional continuity — solving the genuine problem that no other state reliably protected Jews from expulsion, pogrom, or genocide, and that minority status everywhere else had repeatedly proven fatal.
% TRANSFER_FUNCTION: Moves land, residency rights, and political sovereignty from the Arab majority population present in Mandate Palestine to Jewish immigrants and their descendants and the state they founded, financed partly by diaspora fundraising and mandate-era and later great-power diplomatic backing.
% ABSENT_VOICES: Palestinian Arab residents and their descendants are present in the history but structurally excluded from the reading's own legitimacy calculus: their prior residence and national claim are treated as either administrative fact (Mandate era) or security threat (post-1948) rather than as an indigeneity claim of comparable weight to the Jewish one this reading centers.
% DISAPPEARANCE_RATIONALE: If the national-liberation reading's legitimating force disappeared, the state's diplomatic, legal, and moral defense architecture (UN recognition rationale, Law of Return, US and Western alliance justification, diaspora fundraising narrative) would need a different grounding; land and citizenship policy debates would open onto the settler-colonial and religious-restoration accounts directly, and the refugee right-of-return question would lose its principal rhetorical counter.
% FOUNDING_PROBLEM: Sustained, escalating persecution of Jews in Europe and elsewhere — pogroms, legal exclusion, and ultimately genocide — with no state anywhere reliably guaranteeing safety, producing a movement to establish sovereign self-defense and refuge in the ancestral homeland.
% FOUNDING_PROBLEM_CORROBORATION: Holocaust and pre-Holocaust persecution history is corroborated extensively by non-Jewish historians, international courts (Nuremberg), and independent archival research — the underlying persecution is not seriously disputed. What is contested by historians outside the movement's own institutions (including Israeli 'new historians' such as Benny Morris and Ilan Pappe, and Palestinian and international scholars) is whether the SOLUTION adopted — sovereignty over a land with an existing Arab majority — was the necessary or exclusive remedy, and whether the founding problem (statelessness and persecution) remains equally live today as justification for the specific displacement outcomes that followed.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__national_liberation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__national_liberation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply from 1917 (Balfour Declaration, immigration acceleration) through 1948 (mass displacement, war) and stays substantially elevated afterward — the founding coordination benefit (refuge) is front-loaded in 1897-1917 while the extractive cost (displacement, refugee status, denial of return) compounds from 1948 onward and has not been resolved. Suppression tracks a similar arc: minimal when the movement was a minority immigration current, spiking sharply around 1948 when military and administrative enforcement became necessary to establish and hold the state against local and regional resistance, and remaining elevated through subsequent decades of occupation, blockade, and settlement expansion. Theater ratio stays comparatively low and stable — the coordination function (statehood, refuge, defense) is substantially real and operative, not primarily performative, which is part of why this reading resists collapsing cleanly into snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish returnees and settlers and the state apparatus sit toward the beneficiary end: the constraint's core function (sovereignty, refuge, land access) accrues to them, even though many individual settlers/soldiers also bore real personal costs (war, loss). Palestinian Arab residents and their refugee descendants sit at the target end: trapped exit options (no state recognizes a return right symmetrical to the Law of Return), powerless in the relevant period, and the reading's own logic treats their objection as denial of rights rather than as a counter-claim — which is precisely the structural feature that pushes their directionality toward full target rather than symmetric. Diaspora communities benefit substantially with mobile exit options, insulating them from the reading's costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a stateless, persecuted people needing guaranteed refuge) was genuinely live in 1897-1948 and is not simply theater — Jewish statelessness had produced catastrophic, repeatedly demonstrated costs. That is why this reading does not compute as a pure snare: there is a real coordination function underneath it. But mandatrophy risk appears in the founding_problem_status: contested — the specific remedy chosen (sovereignty over a majority-Arab land) is treated by the reading's own framework as permanently vindicated by the original persecution, even as the displaced population's claims persist across generations and the original emergency (imminent genocide) is not the same in 2024 as in 1897-1948. Treating a founding emergency as eternally justificatory, regardless of how conditions or the balance of harms have shifted, is the mandatrophy signature this reading is exposed to — and is exactly the structural delta that distinguishes it from the settler_colonial_reading, which treats the same events as never having had legitimate coordination content.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persecution_justifies_displacement_scope,
    'Does severe persecution of a people justify displacement of a third population with its own prior claim to the same land, and if so, for how long does that justification remain operative?',
    'No empirical resolution mechanism exists; this is a normative question about whether historical injury to one people can license current and ongoing costs to an unconnected third population, and whether that license has a natural expiration.',
    'If the justification is time-bound to the original emergency period (1897-1948), most of the post-1948 extraction becomes harder to legitimate under this reading''s own terms. If unbounded, the reading''s extraction claim remains stable indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(persecution_justifies_displacement_scope, preference, 'Whether persecution-based justification for displacement has a natural time horizon.').

omega_variable(
    arab_opposition_as_rights_denial_vs_rival_claim,
    'Is Palestinian Arab opposition to Jewish immigration and statehood structurally a denial of Jewish national rights (this reading''s premise), or is it a rival indigeneity and self-determination claim of comparable standing (the settler_colonial_reading''s premise)?',
    'This is the central kernel-level disagreement and is not resolvable by additional historical fact-finding alone; it depends on whether indigeneity and self-determination claims are treated as commensurable across the two populations or as asymmetric based on the history of persecution each brings to the claim.',
    'If treated as commensurable, this reading''s core premise collapses into something closer to the settler_colonial_reading''s framing, and the extraction measured here would be read as unjustified rather than justified-but-costly. If treated as asymmetric by persecution history, this reading''s legitimacy claim holds as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(arab_opposition_as_rights_denial_vs_rival_claim, conceptual, 'Whether Arab opposition counts as rights-denial or as a rival legitimate national claim — the kernel''s central fault line.').

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (existential Jewish statelessness and persecution risk) still live in its 1897-1948 form today, or has it been substantially resolved by the existence of the state itself, such that continued displacement-linked policies (settlement expansion, refugee non-return) now serve a different, less urgent function?',
    'Comparative analysis of Jewish safety and statehood alternatives pre-1948 versus post-1948, and assessment of whether current policies are defensible as continuations of the original emergency response or as a distinct project.',
    'If the founding problem is substantially resolved, current extraction (settlement, refugee non-return) reads as detached from its original justification — supporting a mandatrophy finding. If the problem is judged still live (e.g., ongoing antisemitism, absence of alternative guaranteed refuge), the reading''s justification remains more directly connected to present conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the original persecution-based emergency still justifies current-era policy under this reading''s own terms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__national_liberation_reading, 1897, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1897, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1897, 0.1).
narrative_ontology:measurement_basis(zion_tr_t1897, observed).
narrative_ontology:measurement(zion_tr_t1917, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement_basis(zion_tr_t1917, observed).
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1948, 0.18).
narrative_ontology:measurement_basis(zion_tr_t1948, observed).
narrative_ontology:measurement(zion_tr_t1967, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement_basis(zion_tr_t1967, observed).
narrative_ontology:measurement(zion_tr_t1993, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1993, 0.22).
narrative_ontology:measurement_basis(zion_tr_t1993, observed).
narrative_ontology:measurement(zion_tr_t2024, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 2024, 0.2).
narrative_ontology:measurement_basis(zion_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(zion_be_t1897, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1897, 0.15).
narrative_ontology:measurement_basis(zion_be_t1897, observed).
narrative_ontology:measurement(zion_be_t1917, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1917, 0.28).
narrative_ontology:measurement_basis(zion_be_t1917, observed).
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1948, 0.62).
narrative_ontology:measurement_basis(zion_be_t1948, observed).
narrative_ontology:measurement(zion_be_t1967, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement_basis(zion_be_t1967, observed).
narrative_ontology:measurement(zion_be_t1993, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement_basis(zion_be_t1993, observed).
narrative_ontology:measurement(zion_be_t2024, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(zion_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1897, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1897, 0.1).
narrative_ontology:measurement_basis(zion_su_t1897, observed).
narrative_ontology:measurement(zion_su_t1917, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1917, 0.25).
narrative_ontology:measurement_basis(zion_su_t1917, observed).
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement_basis(zion_su_t1948, observed).
narrative_ontology:measurement(zion_su_t1967, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1967, 0.6).
narrative_ontology:measurement_basis(zion_su_t1967, observed).
narrative_ontology:measurement(zion_su_t1993, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1993, 0.52).
narrative_ontology:measurement_basis(zion_su_t1993, observed).
narrative_ontology:measurement(zion_su_t2024, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 2024, 0.55).
narrative_ontology:measurement_basis(zion_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__national_liberation_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, religious_restoration_reading).

% DUAL FORMULATION NOTE:
% This story, settler_colonial_reading, and religious_restoration_reading form a three-member constraint family reading the same kernel (zionist_legitimacy_basis) from incompatible foundational premises. national_liberation_reading centers persecution and historical/ancestral connection as the legitimating basis and reads Arab opposition as illegitimate denial of rights; settler_colonial_reading centers indigenous displacement by an externally-organized colonial movement and reads the same events as dispossession without legitimating coordination function; religious_restoration_reading centers divine promise and messianic fulfillment (particularly salient post-1967) and treats territorial expansion itself as religiously mandated rather than defensively necessitated. Each carries its own ε, beneficiary/victim structure, and claimed_type; none is a hedge or average of the others. All three link to each other via affects_constraints per the ε-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
