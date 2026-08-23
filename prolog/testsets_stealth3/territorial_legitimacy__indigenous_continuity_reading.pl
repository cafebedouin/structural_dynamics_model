% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__indigenous_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__indigenous_continuity_reading, []).

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
 *   constraint_id: territorial_legitimacy__indigenous_continuity_reading
 *   human_readable: Territorial Legitimacy — Indigenous Continuity Reading (1948 as Nakba)
 *   domain: political_theory/international_law
 *
 * SUMMARY:
 *   A legitimacy standard for sovereignty over historic Palestine that
 *   grounds title in continuous indigenous habitation and in
 *   self-determination exercised against colonial dispossession. Under this
 *   standard the events of 1948 are constitutive rather than incidental: the
 *   displacement of roughly three-quarters of a million Palestinians, the
 *   destruction of their towns and villages, and the bar on their return are
 *   the founding fact against which any territorial order is measured. The
 *   standing arrangement — a state whose sovereignty the standard classifies
 *   as settler-colonial in origin, governing territory from which the
 *   indigenous population was expelled and is still excluded — is assessed by
 *   this reading's own lights as maximally extractive: it rests on land,
 *   property, and political standing taken from a population whose
 *   restitution remains the central unsettled debt. The constraint's
 *   operative work is to keep that assessment alive and actionable: to
 *   sustain a unified claim across a stateless and dispersed people, to
 *   anchor the right of return as the test of any settlement, and to deny
 *   legitimacy to arrangements that would close the account without
 *   restitution. Metric referents follow the kernel-reading convention:
 *   extractiveness is authored for the standing arrangement under contest as
 *   this reading assesses it; suppression, theater, collapse, and resistance
 *   describe the constraint's own operation. Claim and metrics are authored
 *   independently: the claimed type states the structure judged true of the
 *   constraint; the metrics state what is judged descriptively so.
 *
 * KEY AGENTS:
 *   - palestinian_refugees_descendants: the claim's core constituency (moderate/identity_locked) — bears the standing arrangement's costs; the framework's principal intended beneficiary
 *   - palestinian_national_institutions: agenda setter (organized/constrained) — administers the claim, speaks for it internationally, and polices its boundaries
 *   - jewish_israeli_population: primary target of the framework's categorical delegitimation (powerful/identity_locked)
 *   - palestinian_normalization_advocates: internal payers (moderate/constrained) — sanctioned for compromise advocacy
 *   - unrwa_registration_administration: co-agenda setter (institutional/constrained) — administers the descendant-transmitted registry the continuity claim runs on
 *   - arab_host_states: secondary beneficiaries (institutional/mobile) — monetize the unresolved claim while restricting refugee integration
 *   - decolonial_solidarity_networks: carriers and incidental beneficiaries (organized/mobile)
 *   - mizrahi_expellee_communities: excluded voice (moderate/identity_locked) — misclassified by the framework's settler/indigenous binary
 *   - international_legal_forums: adjudicating observers (institutional/analytical)
 *   - comparative_settler_colonial_scholars: analytical observers (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.84).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.69).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.69).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Territorial Legitimacy — Indigenous Continuity Reading (1948 as Nakba)").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political_theory/international_law").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, '2cfb9a30-74c9-4ecc-b7a9-2db4784cbf8c').
narrative_ontology:cs_kernel_codification('2cfb9a30-74c9-4ecc-b7a9-2db4784cbf8c', distributed).
narrative_ontology:cs_authority_grounding('2cfb9a30-74c9-4ecc-b7a9-2db4784cbf8c', distributed).
narrative_ontology:cs_reading_relation('2cfb9a30-74c9-4ecc-b7a9-2db4784cbf8c', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('2cfb9a30-74c9-4ecc-b7a9-2db4784cbf8c', territorial_legitimacy__security_necessity_reading, forecloses).
narrative_ontology:cs_axiom('2cfb9a30-74c9-4ecc-b7a9-2db4784cbf8c', foundational, indigenous_continuous_habitation_confers_title).
narrative_ontology:cs_axiom_status(indigenous_continuous_habitation_confers_title, holdable).
narrative_ontology:cs_axiom_grounding('2cfb9a30-74c9-4ecc-b7a9-2db4784cbf8c', indigenous_continuous_habitation_confers_title, deontological).
narrative_ontology:cs_axiom('2cfb9a30-74c9-4ecc-b7a9-2db4784cbf8c', foundational, settler_colonial_origin_voids_sovereignty).
narrative_ontology:cs_axiom_status(settler_colonial_origin_voids_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('2cfb9a30-74c9-4ecc-b7a9-2db4784cbf8c', settler_colonial_origin_voids_sovereignty, deontological).
narrative_ontology:cs_axiom('2cfb9a30-74c9-4ecc-b7a9-2db4784cbf8c', secondary, right_of_return_non_negotiable_precondition).
narrative_ontology:cs_axiom_status(right_of_return_non_negotiable_precondition, holdable).
narrative_ontology:cs_axiom_grounding('2cfb9a30-74c9-4ecc-b7a9-2db4784cbf8c', right_of_return_non_negotiable_precondition, deontological).
narrative_ontology:cs_reference_frame('2cfb9a30-74c9-4ecc-b7a9-2db4784cbf8c', indigenous_habitation_self_determination_baseline).
narrative_ontology:cs_drift_state('2cfb9a30-74c9-4ecc-b7a9-2db4784cbf8c', contemporary_post_2023_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2cfb9a30-74c9-4ecc-b7a9-2db4784cbf8c', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugees_descendants).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, palestinian_national_institutions).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, arab_host_states).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, decolonial_solidarity_networks).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, jewish_israeli_population).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_normalization_advocates).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, anti_colonial_self_determination_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, ga_resolution_194_return_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Families displaced in the 1948 war and their descendants, registered as refugees and living in camps and diaspora communities across the region and beyond. The framework names their dispossession as the founding event of the territorial order and centers their return as the measure of any legitimate settlement. Leaving the framework would mean letting the claim to homes and villages lapse — for many, an abandonment of family dead and destroyed villages that feels like a second dispossession. Materially they hold little state power; their leverage runs through representation, litigation, and solidarity networks.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugees_descendants, beneficiary,
    moderate, generational, identity_locked, global).

% The PLO and successor bodies speak for the claim in international forums, administer refugee affairs, run commemoration and education, and police cooperation with the standing arrangement inside their constituencies. Their mandates, budgets, and diplomatic standing depend on the unresolved claim continuing to organize the field; a settled territorial bargain would shrink their reason to exist. They both serve the constituency and manage it.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_national_institutions, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, palestinian_national_institutions, beneficiary).

% Citizens of the state whose founding the framework dates to the 1948 displacement and classifies as settler-colonial. Individuals born decades after the war, including descendants of refugees from other countries, inherit the categorical designation regardless of personal or family history. Their collective life — language, army, economy, cemeteries — is rooted in the territory the framework says was taken; exiting the designation would mean dissolving the collective project itself rather than moving somewhere. They hold a state, an army, and firm international patrons, so the framework's costs reach them as delegitimation and pressure rather than immediate material loss.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, jewish_israeli_population, payer,
    powerful, generational, identity_locked, national).

% Palestinians who argue for partition-based settlements, mutual recognition, or cooperative arrangements with the standing arrangement. Inside the framework's logic their position reads as surrender of the return claim, and they pay for it in social sanction, lost standing in institutions, and accusations of betrayal. Their exit is inward: they cannot leave the community whose fate they are arguing about.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_normalization_advocates, payer,
    moderate, biographical, constrained, regional).

% The relief agency that registers refugees and — uniquely among refugee regimes worldwide — transmits refugee status to descendants, keeping the displaced population enumerated, serviced, and concentrated across generations. Its rolls are the demographic backbone of the continuity claim; its budget crises and mandate renewals are recurring flashpoints. It did not create the framework but administers the substrate it runs on.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, unrwa_registration_administration, agenda_setter,
    institutional, generational, constrained, regional).

% States that received the displaced in 1948 and after. They invoke the claim in regional diplomacy and against the standing arrangement, while several restrict refugees' employment, citizenship, and property rights in ways that keep the population separate and the claim's constituency intact. They can and sometimes do exit the confrontation entirely by normalizing relations, as two of them have.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, arab_host_states, beneficiary,
    institutional, generational, mobile, regional).

% Activists, academics, and organizations outside the region who carry the framework into universities, unions, courts, and street politics. The framework supplies their analytic categories and campaign targets; sustained engagement builds careers, publications, and organizational memberships. They can reduce involvement at low cost, though accumulated reputational investment creates stickiness.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, decolonial_solidarity_networks, beneficiary,
    organized, biographical, mobile, global).

% Jewish communities expelled or fled from Arab and Muslim countries around 1948–1951, numbering several hundred thousand, most of whom resettled in Israel. The framework's settler/indigenous binary assigns them to the settler side of its ledger even though they arrived as refugees stripped of property. They are largely absent from the framework's conferences, curricula, and adjudications; their objection — that the binary cannot classify them — is rarely heard where the framework is authoritative.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, mizrahi_expellee_communities, excluded,
    moderate, generational, identity_locked, regional).

% UN bodies, the International Court of Justice, and treaty committees that periodically adopt, cite, or decline the framework's categories. They supply resolutions, opinions, and procedural venues that the claim's carriers treat as warrants and opponents treat as politicized. They adjudicate without enforcing; their outputs shift legitimacy accounting at the margins.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_legal_forums, observer,
    institutional, generational, analytical, global).

% Analysts who study the framework alongside other settler-colonial and post-imperial cases. They observe the full structure — what the framework coordinates, whom it costs, how its enforcement evolves — without holding a seat in the dispute.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, comparative_settler_colonial_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__indigenous_continuity_reading, palestinian_national_institutions).
narrative_ontology:fixing_cost_class(territorial_legitimacy__indigenous_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a stateless, dispersed, and administratively separated population nationally legible as one people with one outstanding claim: a shared account of the founding event, a shared standard for judging territorial proposals, and an intergenerational registry of who is owed what. It solves the collective-action problem of sustaining a restitution claim across generations of exile, when every ordinary pressure — time, local integration, turnover of governments — works toward forgetting.
% TRANSFER_FUNCTION: Moves legitimacy-status and moral standing from the standing sovereign arrangement to the indigenous claim; moves attention, advocacy labor, and institutional resources toward the return demand; and moves categorical standing away from the population the framework classes as settlers and away from community members who advocate closing the account short of return.
% ABSENT_VOICES: Mizrahi expellee communities — Jews displaced from Arab countries in the same years, whom the settler/indigenous binary classifies against their own history — are structurally absent from the framework's venues. Also under-heard: Jewish Israelis who would accept full civic equality but reject categorical delegitimation, and Palestinians who privately favor partition but stay silent under normalization stigma. They are absent because the framework's authoritative spaces — its conferences, curricula, and movement disciplines — presuppose the binary their objections unsettle.
% DISAPPEARANCE_RATIONALE: Without the framework, the Palestinian national claim loses its unifying standard: diaspora communities drift toward local integration, host states lose their principal diplomatic instrument against the standing arrangement and would face integration pressure for populations they have kept separate, the right of return loses its legal-moral anchor and with it the largest outstanding property-restitution claim of the century, and contestation of the standing arrangement reorganizes around the rival readings. The regional order rearranges around whichever frame fills the vacuum.
% FOUNDING_PROBLEM: The 1948 war and its aftermath: the displacement of most of the Palestinian population, the destruction of hundreds of villages, the seizure of land and property, and the permanent barring of return — leaving a people stateless, scattered across host states, and without any framework in which their continuity or restitution could even be asserted.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: UN General Assembly Resolution 194 (III) of December 1948 — adopted before the framework consolidated — affirms the return and restitution principle; the Israeli state's own archives, opened to independent historians from the late 1980s, document the expulsions and the bar on return; host-state and Red Cross records corroborate the scale of displacement. No serious party disputes that the displacement happened; the live dispute is over its moral-legal consequence, which is precisely the kernel contest.
narrative_ontology:disappearance_verdict(territorial_legitimacy__indigenous_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__indigenous_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy__indigenous_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__indigenous_continuity_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.84) is authored for the standing arrangement as this reading assesses it: mass displacement never remedied, expropriated property never restored, and a closing window of living witnesses — near the top of the scale on the reading's own terms. Suppression (0.69) describes the constraint's own enforcement: mostly social and internalized rather than carceral — anti-normalization sanctioning, boundary-policing of acceptable positions, and identity fusion that makes exit feel like ancestral betrayal — with a structural layer in institutional exclusion and funding consequences. Theater (0.52) has risen past the Goodhart threshold: decades of commemoration, symbolic recognition, and resolution-passing have increasingly substituted for material progress on return, and the annual cycle now performs the claim as much as it advances it. Accessibility collapse (0.60) is moderate-high: within the framework's logic, compromise alternatives collapse completely — a partition that closes the return account is not a live option inside this frame — but the lived world retains them, so the collapse is logical rather than practical. Resistance (0.72) is high and organized: the framework meets counter-campaigns, hostile legislation, and two rival legitimacy readings with institutional backing. The temporal series share one grid. Extractiveness oscillates around a secular high plateau, spiking with wars (1967, 1988, 2000, 2023–26) and easing in their aftermath; the oscillation is a mobilization rhythm rather than noise, and the anniversary cycle functions as intermittent reinforcement of identity commitment. The suppression_requirement series is authored deliberately: the story traces enforcement-capacity build-up — from spontaneous commemoration in 1948 to today's registration systems, curricula, litigation, and campus enforcement — a ratcheting machinery that now holds the line as much as grievance does.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently because the same structure is a lifeline, a cage, and a discipline depending on position. From the refugee seat the constraint is the only thing keeping a dissolved society legible as a nation with a claim — coordination experienced as survival. From the Israeli seat the same structure is categorical exclusion: it assigns every member, including those born decades later and descended from other refugee streams, to a settler class whose collective existence has no legitimate form — a target experience amplified by identity lock, since exit would mean dissolving the collective self rather than relocating. From the compromiser seat it is internal discipline: the framework prices deviation as betrayal. Host states experience it as a diplomatic asset they can monetize or abandon at will — the mobile-exit seat that damps their extraction exposure. Identity fusion differs by mechanism: ancestral-relational for refugees (keys, village names, family dead), national-institutional for Israelis (army, language, cemeteries), career-analytical for solidarity carriers. If the refugee identity frame broke — descendants reclassifying themselves as locals of where they live — the constituency would hollow within a generation; if the Israeli frame broke, the categorical cost would lose its object.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: refugees and descendants are the constraint's intended beneficiaries — the framework subsidizes their claim and imposes no cost on them for holding it; national institutions sit lower still, since they both serve and capture it; host states and solidarity networks benefit incidentally and hold mobile or cheap exits, damping their exposure. Victim declarations map to high directionality: the Israeli public bears the constraint's categorical status cost with identity-locked exit, placing it near the full-target end despite its real-world power — power shapes retaliation capacity, not exposure to this particular structure. Compromise advocates inside the claimant community bear enforcement costs with constrained exit. Spatial scope is effectively global (diaspora, forums, campuses), which scales verification difficulty and thus effective extraction upward for the targeted seats. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already yield the correct ordering, and the override surface keys on power atoms too coarse to improve on the structural derivation here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a dispossessed people with no framework to sustain a sovereignty claim across generations of exile — is live: the displacement persists, the refugees' descendants remain unreturned, and no settlement has closed the account. Live status paired with a world_rearranges disappearance verdict is the consistent combination; no zombie mismatch fires. The classification's protective work is bidirectional: reading the constraint as a snare would erase the genuine coordination that keeps a stateless society nationally legible — the coordination function is primary and sincerely held, not cover; reading it as a rope would hide the categorical asymmetric costs imposed on the other population and on internal dissent, and the enforcement machinery those costs require. Tangled rope holds both truths. The forward risk is different: if restitution becomes administratively unreachable — property liquidated, villages built over, witness generations gone — the framework's maintenance could drift from claim-advancing to claim-performing, which is the trajectory the theater series (now past 0.5) begins to register. That is a piton-risk watch item, not a current classification: the enforcement build-up shows the structure is still being actively held, not merely performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the indigenous_continuity_reading of the territorial_legitimacy kernel. What changes structurally if a sibling reading is adopted instead?',
    'Compare the compiled stories for partition_reading and security_necessity_reading: victim-set composition, epsilon for the standing arrangement, and per-seat classifications under each reading.',
    'Under partition_reading the standing arrangement''s epsilon drops sharply and the victim set narrows to border-crossing violations; under security_necessity_reading the standing arrangement becomes the legitimate baseline and this reading''s constituency becomes the contesting party. This story''s classification holds only within this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling adoption relocates epsilon and victim sets.').

omega_variable(
    disagreement_location_1948_status,
    'At what structural element of the legitimacy question do the readings of this kernel actually disagree?',
    'Locate the divergence at the moral-legal status assigned to the 1948 displacement: foundational injustice demanding restitution (this reading), legally superseded population movement (partition reading), or wartime necessity (security reading). Tracing each reading''s treatment of that single element maps the whole contest.',
    'If the disagreement is located at 1948''s status rather than at borders or recognition, mediation formats that bracket 1948 cannot satisfy this reading''s carriers, and the constraint''s enforcement will keep returning to the founding event regardless of territorial bargains struck elsewhere.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_1948_status, conceptual, 'The kernel contest concentrates on the status of the 1948 displacement.').

omega_variable(
    indigeneity_criterion_selectivity,
    'Is the continuous-habitation legitimacy criterion applied as a universal principle or selectively to this case?',
    'Comparative audit: apply the criterion to other settler-states and post-imperial arrangements (the Americas, Australia, South Africa''s transition) and test whether the framework''s carriers accept the results.',
    'Selective application indicates the criterion functions as a partisan instrument, raising the constraint''s effective extraction beyond the authored base; universal application makes it a general legitimacy principle with a far wider demolition radius than any single dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigeneity_criterion_selectivity, conceptual, 'Universality versus selectivity of the habitation criterion.').

omega_variable(
    return_commitment_reproduction_mechanism,
    'Is the intergenerational commitment to return sustained by lived grievance or by institutionalized identity reproduction (registration, curriculum, commemoration)?',
    'Longitudinal attitude surveys across refugee generations; natural experiments where registration or service administration changed; comparison with displaced populations whose status administration lapsed.',
    'If reproduction is chiefly institutional, the constraint''s persistence decouples from the founding grievance, pushing theater_ratio higher and pointing toward inertial drift once the witness generations pass; if lived, the constraint stays grievance-anchored and enforcement requirements remain tied to events.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(return_commitment_reproduction_mechanism, empirical, 'Lived-grievance versus institutionally-reproduced commitment to return.').

omega_variable(
    anti_normalization_suppression_mechanism,
    'Is the suppression of compromise advocacy inside the claimant community structural (material sanction, institutional exclusion) or internalized (dissenters self-censor, having absorbed the betrayal frame)?',
    'Post-exit trajectory: track advocates who leave the framework''s venues — if self-censorship persists where the sanction machinery is absent, the suppression is substantially internalized.',
    'Internalized suppression means the constraint''s effective suppression exceeds the structural measure and travels with dissenters after exit; structural suppression means enforcement capacity, not conviction, holds the line — cheaper to relax if enforcement withdraws.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anti_normalization_suppression_mechanism, empirical, 'Structural versus internalized anti-normalization enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 1948, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1948, 0.14).
narrative_ontology:measurement(terr_tr_t1958, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1958, 0.19).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1967, 0.27).
narrative_ontology:measurement(terr_tr_t1977, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1977, 0.33).
narrative_ontology:measurement(terr_tr_t1988, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1988, 0.39).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2000, 0.43).
narrative_ontology:measurement(terr_tr_t2011, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2011, 0.48).
narrative_ontology:measurement(terr_tr_t2026, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2026, 0.52).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1948, 0.86).
narrative_ontology:measurement(terr_be_t1958, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1958, 0.71).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1967, 0.78).
narrative_ontology:measurement(terr_be_t1977, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1977, 0.74).
narrative_ontology:measurement(terr_be_t1988, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1988, 0.77).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(terr_be_t2011, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2011, 0.82).
narrative_ontology:measurement(terr_be_t2026, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2026, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1948, 0.24).
narrative_ontology:measurement(terr_su_t1958, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1958, 0.31).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1967, 0.37).
narrative_ontology:measurement(terr_su_t1977, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1977, 0.45).
narrative_ontology:measurement(terr_su_t1988, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1988, 0.54).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2000, 0.61).
narrative_ontology:measurement(terr_su_t2011, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2011, 0.66).
narrative_ontology:measurement(terr_su_t2026, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2026, 0.69).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__security_necessity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the legitimacy question of Israel/Palestine' covers three structurally distinct constraints — one per reading of the territorial_legitimacy kernel. They differ in epsilon (this reading assesses the standing arrangement as near-maximally extractive settler colonialism; the partition reading assesses a recognized-borders arrangement as modestly extractive; the security reading treats the standing arrangement as the legitimate baseline), in victim sets (displaced Palestinians and their descendants versus cross-border violation sufferers versus threatened populations), and in failure modes. This file links both siblings; citation runs in both directions, since each reading cites the world the others made. Per the epsilon-invariance principle, no single story could carry all three: measuring legitimacy through habitation, through recognition, and through security yields different epsilon values for the same territory, which is the signature of three constraints sharing one colloquial label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
