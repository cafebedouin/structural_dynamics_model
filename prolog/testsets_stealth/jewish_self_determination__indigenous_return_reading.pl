% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__indigenous_return_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__indigenous_return_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: jewish_self_determination__indigenous_return_reading
 *   human_readable: Indigenous Return Reading of Jewish Self-Determination
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This story models the indigenous-return reading of Jewish
 *   self-determination as a standing discursive arrangement: the claim that
 *   the Jewish people are indigenous to the land with unbroken connection,
 *   and that Zionism is therefore decolonization rather than colonization.
 *   The arrangement operates wherever legitimacy for Jewish territorial
 *   sovereignty is adjudicated — legislatures, campuses, courts,
 *   international bodies — and it does two things at once: it anchors
 *   collective identity in a documented historical substrate (origin,
 *   liturgical orientation, continuous presence, Hebrew revival), and it
 *   converts that substrate into a title-conferring inference that
 *   subordinates competing attachment to the land and preempts the
 *   settler-colonial classification. The claim and the metrics are authored
 *   independently: the reading presents the constraint as historical fact
 *   (hence the mountain claim with emerges_naturally), while the authored
 *   metrics describe heavily contested, actively enforced operation with
 *   identifiable beneficiaries and cost-bearers — exactly the profile
 *   false-summit detection exists to catch. Per the epsilon-invariance
 *   principle, the empirical substrate is decomposed into a companion story
 *   (jewish_levantine_origin_historical_record) cited upstream; the four
 *   sibling readings of the same kernel are separate constraint files linked
 *   through the network section, never folded into this one.
 *
 * KEY AGENTS:
 *   - israeli_state_institutions: agenda-setter (institutional/constrained) — codifies and defends the framing in law, curriculum, and diplomacy; collects the legitimacy the claim underwrites
 *   - zionist_advocacy_networks: primary beneficiary (organized/identity_locked) — propagates and defends the claim abroad; organizational existence fused to it
 *   - diaspora_jewish_communities: beneficiary with payer exposure (moderate/identity_locked) — receives the dignity-and-insurance narrative; absorbs spillover hostility and conscripted loyalty
 *   - palestinian_indigenous_claimants: primary target (moderate/trapped) — bears subordination of their own attachment and dispossession account in every forum
 *   - jewish_diasporist_dissenters: secondary target (moderate/identity_locked) — bound into a national narrative spoken in their name without consent; dissent sanctioned
 *   - occupied_palestinian_witnesses: excluded voice (powerless/trapped) — testimony admissible only after translation into the frame's terms
 *   - indigenous_rights_bodies: analytical observer (institutional/analytical) — adjudicate whether the indigeneity category validly extends to this case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, 0.74).
domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, 0.78).
domain_priors:theater_ratio(jewish_self_determination__indigenous_return_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__indigenous_return_reading, mountain).
narrative_ontology:human_readable(jewish_self_determination__indigenous_return_reading, "Indigenous Return Reading of Jewish Self-Determination").
narrative_ontology:topic_domain(jewish_self_determination__indigenous_return_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__indigenous_return_reading).
domain_priors:emerges_naturally(jewish_self_determination__indigenous_return_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__indigenous_return_reading, '7033dfbe-fb35-40b4-bb33-aa6cb1ca04d0').
narrative_ontology:cs_kernel_codification('7033dfbe-fb35-40b4-bb33-aa6cb1ca04d0', formalized).
narrative_ontology:cs_authority_grounding('7033dfbe-fb35-40b4-bb33-aa6cb1ca04d0', lineage).
narrative_ontology:cs_interpretation_layer_present('7033dfbe-fb35-40b4-bb33-aa6cb1ca04d0').
narrative_ontology:cs_reading_relation('7033dfbe-fb35-40b4-bb33-aa6cb1ca04d0', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('7033dfbe-fb35-40b4-bb33-aa6cb1ca04d0', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('7033dfbe-fb35-40b4-bb33-aa6cb1ca04d0', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7033dfbe-fb35-40b4-bb33-aa6cb1ca04d0', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('7033dfbe-fb35-40b4-bb33-aa6cb1ca04d0', foundational, unbroken_connection_confers_return_title).
narrative_ontology:cs_axiom_status(unbroken_connection_confers_return_title, holdable).
narrative_ontology:cs_axiom_grounding('7033dfbe-fb35-40b4-bb33-aa6cb1ca04d0', unbroken_connection_confers_return_title, empirically_contingent).
narrative_ontology:cs_axiom('7033dfbe-fb35-40b4-bb33-aa6cb1ca04d0', secondary, decolonization_classification_preempts_settler_frame).
narrative_ontology:cs_axiom_status(decolonization_classification_preempts_settler_frame, holdable).
narrative_ontology:cs_axiom_grounding('7033dfbe-fb35-40b4-bb33-aa6cb1ca04d0', decolonization_classification_preempts_settler_frame, conventional).
narrative_ontology:cs_reference_frame('7033dfbe-fb35-40b4-bb33-aa6cb1ca04d0', unbroken_title_continuity).
narrative_ontology:cs_drift_state('7033dfbe-fb35-40b4-bb33-aa6cb1ca04d0', contemporary_indigenous_studies_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7033dfbe-fb35-40b4-bb33-aa6cb1ca04d0', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__indigenous_return_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, zionist_advocacy_networks).
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_self_determination__indigenous_return_reading, palestinian_indigenous_claimants).
narrative_ontology:constraint_victim(jewish_self_determination__indigenous_return_reading, jewish_diasporist_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__indigenous_return_reading, diaspora_jewish_communities).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, unbroken_connection_principle).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, indigeneity_confers_return_title).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, decolonization_through_return_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codifies the narrative in constitutional instruments (the 2018 Nation-State Law declares Jewish settlement a national value and Hebrew the national language), teaches it in the school curriculum, and deploys it diplomatically whenever the state's founding is challenged. The narrative underwrites the state's answer to the question of why Jewish sovereignty here is legitimate. Softening the claim's exclusivity — acknowledging equivalent standing for Palestinian attachment — would reopen the state's foundational justification to its strongest critique, so the institutions defend the full-strength version even where a weaker version would serve.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Organizations devoted to explaining and defending the claim abroad: campus outreach, media monitoring, legislative testimony, litigation support. Their budgets, membership, and purpose track the intensity of challenge to the narrative; a world that accepted the framing at face value would need far less of them, while a world that rejected it outright would hand them their cause. Leaving the framing is not organizationally survivable — donors, staff identities, and institutional memory are built around it.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, zionist_advocacy_networks, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__indigenous_return_reading, zionist_advocacy_networks, agenda_setter).

% Receive the narrative as dignity and insurance: a people with a home and an unbroken story is harder to persecute, and communal schools, synagogues, and federations teach it as the antidote to diaspora vulnerability. The same narrative conscripts them: communal institutions expect public identification with the state's legitimacy, criticism of state policy is socially costly inside the community, and hostility to state policy lands on them as individuals outside it. Declining the narrative means losing standing in the institutions that structure communal life.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, diaspora_jewish_communities, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__indigenous_return_reading, diaspora_jewish_communities, payer).

% Live on and beside the land the narrative claims, with their own continuous presence, place-names, dialects, and descent lines. Under the arrangement their attachment is admitted only in subordinate terms — later arrival, or co-presence without equal title — and their account of 1948 displacement is recast as a rejection of indigenous return. They cannot opt out of the frame: it governs how their testimony is received in every forum where the legitimacy question is argued, and refusing its terms costs them the hearing itself. Representation is fragmented across a citizen minority, an occupied population, and a diaspora, which thins whatever aggregate leverage the class holds.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, palestinian_indigenous_claimants, payer,
    moderate, generational, trapped, global).

% Jews whose traditions of diaspora pluralism, universalism, or anti-nationalism lead them to decline the national narrative. The arrangement speaks indigeneity on their behalf without their consent; dissent reads inside their communities as betrayal or self-erasure, and outside as cover for enemies. Their exit options are poor: leaving the communal frame costs belonging, family standing, and, for professionals in Jewish institutions, livelihood.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, jewish_diasporist_dissenters, payer,
    moderate, biographical, identity_locked, global).

% Residents of the occupied territories whose daily experience — checkpoints, home demolitions, settlement expansion — is the raw material the legitimacy argument processes. They are not seated in the forums where the framing is codified; their testimony reaches those forums only after translation into the frame's terms, and testimony that resists translation is discounted as propaganda.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, occupied_palestinian_witnesses, excluded,
    powerless, immediate, trapped, regional).

% Scholarly and institutional custodians of the indigeneity category — United Nations permanent mechanisms, indigenous-studies associations, comparative-law scholars. They assess whether the category, built for peoples facing living colonial encroachment, validly extends to a return after two millennia. Their determinations carry no enforcement power but set the terms on which the claim travels in international spaces.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, indigenous_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__indigenous_return_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(jewish_self_determination__indigenous_return_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real identity-coordination problem: a dispersed people spanning dozens of polities and languages needs a single intelligible account of who they are and why they may gather somewhere; the claim supplies that account, synchronizes liturgy with politics, directs return migration and philanthropic flows, and gives scattered communities one script for answering the question 'why here?'
% TRANSFER_FUNCTION: Moves narrative authority and legitimacy: from Palestinian claimants (whose attachment is reclassified as subordinate) and from critical scholarship (whose framing is preempted as hostile) toward Jewish national institutions and the state; secondarily it moves diaspora money, votes, and advocacy effort toward the state's defense.
% ABSENT_VOICES: Occupied Palestinian residents whose testimony is admissible only in the frame's terms; Mizrahi Jews whose Arab-Jewish histories sit awkwardly in a Europe-centered return story; diasporist Jews who were never asked. They stand outside the legislatures, curricula boards, and advocacy summits where the framing is codified.
% DISAPPEARANCE_RATIONALE: The state would not dissolve overnight, but its justification architecture would: the basic law's declaratory logic, diaspora fundraising appeals, school curricula, and the entire pro-and-con counter-discourse industry are organized around this claim; within years the legitimacy argument would rebuild around the liberal-nationalist or covenant readings, and the settler-colonial critique would lose its principal foil.
% FOUNDING_PROBLEM: After two millennia of expulsion, ghettoization, and pogroms culminating in the Holocaust, the nation-state system had demonstrated that Jewish safety could not be secured by minority rights alone; the arrangement was built to supply a historical warrant making return to the ancestral land a right rather than a request, so that refuge would not depend on others' permission.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: European Union Fundamental Rights Agency surveys and comparable hate-crime series attest the persistence of the security problem; Holocaust and migration historiography attests the founding predicament; and Palestinian and postcolonial scholars — the constraint's sharpest opponents — concede the historical fact of Jewish origin while disputing the title inference, which corroborates the problem's reality independent of the beneficiary set.
narrative_ontology:disappearance_verdict(jewish_self_determination__indigenous_return_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__indigenous_return_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__indigenous_return_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__indigenous_return_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__indigenous_return_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__indigenous_return_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, ExtMetricName, E),
    domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(jewish_self_determination__indigenous_return_reading),
    narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(jewish_self_determination__indigenous_return_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.74 because the arrangement's operative move — converting a documented historical substrate into an exclusive title that subordinates a rival presence — transfers legitimacy at scale, and because the contested classification itself raises the stakes: every forum fight over the label is a fight the arrangement must win to keep its shield. Suppression (0.78) is authored as a raw structural property, unscaled by power or scope: it combines statutory instruments (anti-boycott statutes across dozens of jurisdictions, funding conditions on institutions hosting the rival framing, the 2018 basic law's declaratory architecture) with communal enforcement (donor pressure, professional consequences). Roughly sixty percent of the measured suppression is structural and forty percent internalized — communal self-policing driven by the historical memory that internal criticism arms external enemies — which is why the suppression-mechanism omega matters: the internalized share would persist through legal reform. Theater_ratio (0.58) reflects a deployment that began as scholarship and has drifted toward repetition: anniversary ceremonies, talking-point cycles, and declaratory legislation increasingly substitute for the archival, archaeological, and linguistic work that once carried the claim. Accessibility_collapse is low (0.25) — understanding the claim collapses nothing; the rival readings flourish in the same journals and courtrooms — and resistance is correspondingly high (0.75). The temporal series run on one shared grid (t=0,25,50,75,100,125); the trajectories are ratchets, not cycles: each war and uprising step-changed enforcement upward and no phase of the record shows relaxation below prior peaks, so no intermittent-reinforcement mechanism is posited.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the agenda-setter seat the arrangement is the state's own founding warrant — challenging it from inside is constitutionally unthinkable, and the seat experiences attack as existential rather than extractive. From the Palestinian payer seat the same structure operates as a hearing filter: their attachment is admitted only in subordinate grammar, a cost paid in every forum where legitimacy is argued. Between them sit two identity-locked hybrid seats: diaspora communities collect dignity and insurance but pay spillover hostility and conscripted loyalty; dissenters pay belonging for declining a narrative spoken in their name. The identity-lock is relational and institutional before it is ideological — communal institutions, donor networks, and professional pathways are built around the narrative, so exit costs are paid in belonging and livelihood rather than in argument. Same-level lateral divergence: diaspora communities and diasporist dissenters hold identical global standing and differ only in whether their identity is fused with the narrative — exit differentiation here tracks identity fusion, not power. If the identity frame broke — if communal institutions decoupled belonging from the state's legitimacy — the diaspora seats' directionality would shift sharply toward the target end and the arrangement's enforcement burden would rise.
 *
 * DIRECTIONALITY LOGIC:
 *   The state institutions sit nearest the beneficiary pole: they collect the legitimacy the claim underwrites and administer its enforcement. Advocacy networks collect budgets and relevance without administering the state — beneficiaries whose secondary enforcement role keeps them adjacent to the agenda-setter seat. Diaspora communities derive low-to-moderate directionality: net beneficiaries whose indirect costs (spillover hostility, conscripted defense) pull them off the pure-beneficiary end. Palestinian claimants sit near the full-target pole: the arrangement's operative act is the subordination of their claim, and they cannot exit the frame in which their testimony is heard. Diasporist dissenters are targets by conscription — the arrangement spends their identity without their consent. Occupied witnesses are excluded rather than targeted: the arrangement filters them out before any flow could register. Indigenous-rights bodies hold the analytical seat: their category rulings move the claim's travel-worthiness without collecting from it. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already separate the seats, and the override surface keys on power atoms too coarsely to distinguish the two moderate-power seats that differ by identity-lock rather than power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that Jewish safety could not be secured by minority rights alone after the Holocaust — is live: hate-crime series and survey data attest continuing antisemitism, and the state's own security doctrine presumes it. Because the founding-problem status is live and the disappearance verdict is world-rearranging, the mismatch consumer finds no dead-mandate flag, and mandatrophy is not resolved. The classification work here runs in both directions: the genuine coordination function (one intelligible identity-account for a dispersed people; a refuge guarantee that does not depend on others' permission) must not be misread as pure extraction — the historical substrate is real and the safety problem is real — and equally the extraction must not be excused as the necessary price of that coordination: the title-conferring inference that subordinates a rival presence does separable work, which is why the fact/inference decomposition is linked upstream. The receipt surface confirms the captured profile: gains concentrate in a named seat and fixing is prohibitive for the seat that could fix it, since softening the claim's exclusivity would reopen the state's foundational justification to its strongest critique.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (indigenous_return_reading) of the jewish_self_determination kernel; which structural elements of the classification flip under each sibling reading?',
    'Cross-reading comparison across the five linked constraint files: align the seat structure, then diff beneficiary/victim sets, epsilon, and computed type per reading.',
    'Under the settler-colonial sibling the beneficiary/victim sets invert and epsilon rises; under the liberal-nationalist sibling the indigeneity grounding drops out and the claim competes as one nation among equals; under the religious-covenant sibling the grounding becomes theological and evidence-resistant; under the diasporist sibling the sovereign arrangement itself becomes the contested object.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this file is one of five readings; every classification value here is reading-indexed, not kernel-level.').

omega_variable(
    natural_fact_vs_constructed_title,
    'Is this constraint a natural-law-like historical fact, or a constructed title-conferring arrangement that benefits identifiable agents?',
    'Separate the empirical substrate (documented origin, continuity, language revival — carried by the upstream companion story) from the normative inference (exclusive title, subordination of rival claims); test whether the inference survives adversarial review under indigenous-studies criteria.',
    'If the inference is constructed, the mountain presentation is a false summit and the constraint computes as a hybrid with concentrated gains and named cost-bearers; if the inference is natural, measured extraction falls toward coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_fact_vs_constructed_title, conceptual, 'Naturality ambiguity: the reading presents the title inference as settled fact; the beneficiaries it concentrates gains on are the false-summit signal.').

omega_variable(
    indigeneity_category_extension_validity,
    'Does the indigeneity category — developed for peoples facing living colonial encroachment — validly extend to a return after a two-millennium diaspora?',
    'Comparative criteria analysis against recognized indigenous cases: language revival, continuous liturgical orientation, physical continuity, colonial-encounter structure, and whether the category presupposes an extant territorial community.',
    'If the extension fails, the decolonization warrant collapses and the arrangement''s extraction rises sharply (cover-story profile); if it succeeds, the coordination-function weight rises and extraction estimates fall materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigeneity_category_extension_validity, conceptual, 'Whether the load-bearing category transfer is legitimate or a borrowing that launders a sovereignty claim.').

omega_variable(
    co_indigeneity_symmetry,
    'Are Palestinian attachment claims co-equal in kind — descent lines, continuous presence, place-name strata — such that the arrangement''s asymmetry is manufactured rather than derived?',
    'Population genetics, onomastic and toponymic stratigraphy, and historiography of continuous southern-Levant presence; comparative legal analysis of how concurrent indigeneity claims are resolved.',
    'If co-equal, the subordination move is the arrangement''s entire extractive payload and the profile sharpens toward pure extraction; if the subordinate-claim account holds, measured extraction falls materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_indigeneity_symmetry, empirical, 'Symmetry of the two attachment claims determines whether the asymmetry is discovered or produced.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression of rival framings structural (statutes, funding conditions, institutional definitions) or internalized (communal self-policing)?',
    'Post-reform trajectory tracking: where statutory barriers fall (courts striking anti-boycott provisions, institutions dropping contested definitions), measure whether dissent rebounds or remains suppressed.',
    'If internalized, effective suppression persists after structural removal and the arrangement''s resilience is understated by the scalar; the working decomposition splits the 0.78 into roughly 0.47 structural and 0.31 internalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural versus internalized suppression share in a mixed-mechanism discursive arrangement.').

omega_variable(
    fact_inference_decomposition,
    'Do the bundled empirical claim (Levantine origin, continuity) and the normative inference (title, exclusivity) share one epsilon?',
    'Compare this story''s epsilon with the upstream companion story jewish_levantine_origin_historical_record, which authors the historical fact alone with no title inference attached.',
    'If the fact-story shows negligible extraction while this story shows high extraction, the payload lives entirely in the inference layer and the family decomposition is validated; convergent values would indicate the bundling itself is load-bearing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fact_inference_decomposition, conceptual, 'Epsilon-invariance check on the fact/inference bundle at the heart of this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__indigenous_return_reading, 0, 125).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsdir_tr_t0, jewish_self_determination__indigenous_return_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(jsdir_tr_t0, observed).
narrative_ontology:measurement(jsdir_tr_t25, jewish_self_determination__indigenous_return_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(jsdir_tr_t25, observed).
narrative_ontology:measurement(jsdir_tr_t50, jewish_self_determination__indigenous_return_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement_basis(jsdir_tr_t50, observed).
narrative_ontology:measurement(jsdir_tr_t75, jewish_self_determination__indigenous_return_reading, theater_ratio, 75, 0.42).
narrative_ontology:measurement_basis(jsdir_tr_t75, observed).
narrative_ontology:measurement(jsdir_tr_t100, jewish_self_determination__indigenous_return_reading, theater_ratio, 100, 0.5).
narrative_ontology:measurement_basis(jsdir_tr_t100, observed).
narrative_ontology:measurement(jsdir_tr_t125, jewish_self_determination__indigenous_return_reading, theater_ratio, 125, 0.58).
narrative_ontology:measurement_basis(jsdir_tr_t125, observed).

% Extraction over time
narrative_ontology:measurement(jsdir_be_t0, jewish_self_determination__indigenous_return_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(jsdir_be_t0, observed).
narrative_ontology:measurement(jsdir_be_t25, jewish_self_determination__indigenous_return_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(jsdir_be_t25, observed).
narrative_ontology:measurement(jsdir_be_t50, jewish_self_determination__indigenous_return_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement_basis(jsdir_be_t50, observed).
narrative_ontology:measurement(jsdir_be_t75, jewish_self_determination__indigenous_return_reading, base_extractiveness, 75, 0.63).
narrative_ontology:measurement_basis(jsdir_be_t75, observed).
narrative_ontology:measurement(jsdir_be_t100, jewish_self_determination__indigenous_return_reading, base_extractiveness, 100, 0.69).
narrative_ontology:measurement_basis(jsdir_be_t100, observed).
narrative_ontology:measurement(jsdir_be_t125, jewish_self_determination__indigenous_return_reading, base_extractiveness, 125, 0.74).
narrative_ontology:measurement_basis(jsdir_be_t125, observed).

% Suppression requirement over time
narrative_ontology:measurement(jsdir_su_t0, jewish_self_determination__indigenous_return_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(jsdir_su_t0, observed).
narrative_ontology:measurement(jsdir_su_t25, jewish_self_determination__indigenous_return_reading, suppression_requirement, 25, 0.28).
narrative_ontology:measurement_basis(jsdir_su_t25, observed).
narrative_ontology:measurement(jsdir_su_t50, jewish_self_determination__indigenous_return_reading, suppression_requirement, 50, 0.44).
narrative_ontology:measurement_basis(jsdir_su_t50, observed).
narrative_ontology:measurement(jsdir_su_t75, jewish_self_determination__indigenous_return_reading, suppression_requirement, 75, 0.56).
narrative_ontology:measurement_basis(jsdir_su_t75, observed).
narrative_ontology:measurement(jsdir_su_t100, jewish_self_determination__indigenous_return_reading, suppression_requirement, 100, 0.68).
narrative_ontology:measurement_basis(jsdir_su_t100, observed).
narrative_ontology:measurement(jsdir_su_t125, jewish_self_determination__indigenous_return_reading, suppression_requirement, 125, 0.78).
narrative_ontology:measurement_basis(jsdir_su_t125, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__indigenous_return_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_levantine_origin_historical_record).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle. The colloquial label 'Jewish indigeneity' bundles two structurally distinct claims: (1) the historical fact of Levantine origin and continuity — authored separately in jewish_levantine_origin_historical_record as a genuine mountain with negligible extraction, cited upstream as evidentiary substrate; and (2) the normative inference that continuity confers overriding political title and a decolonization classification — this file, where the extraction lives. The four sibling readings of the jewish_self_determination kernel are lateral family members, not components: they share the kernel but instantiate different constraints with different beneficiary/victim structures. Upstream influences downstream: the fact-story's empirical confidence is what lends this reading its persuasive force, exactly as an established upstream claim is cited as evidence for a contested downstream one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
