% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__abolitionist_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__abolitionist_rejection, []).

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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Dharmasastra Corpus as Operative Normative Authority (Abolitionist Rejection Reading)
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   The Dharmasastra corpus — dharmasutras, smritis, and their commentaries —
 *   operated for roughly two and a half millennia as South Asia's normative
 *   authority over duty, purity, inheritance, and the varna/jati hierarchy.
 *   This story authors that standing arrangement as the
 *   abolitionist_rejection reading sees it: a constructed domination
 *   machinery with no legitimate residual authority, its coordination story
 *   functioning as cover, its victims enumerable by name. Per the family
 *   decomposition rule, the colloquial label 'Dharmasastra' covers three
 *   structurally distinct constraints sharing one referent: the
 *   orthodox_literalist sibling authors low epsilon over the same referent
 *   (an ordained order whose differential duties are intrinsic, not
 *   transfers), the reformist_contextual sibling authors mid epsilon
 *   (time-bound prescriptions carry the harm; an ethical core is salvagable),
 *   and this reading authors high epsilon (the framework as such is the
 *   oppression). Same referent, reading-indexed values; the stories are
 *   linked through network.affects_constraints. Claim and metrics are
 *   independent authored facts: the snare claim states this reading's
 *   structural verdict; the metrics describe the arrangement's operation as
 *   the historical and sociological record shows it. KEY AGENTS (by
 *   structural relationship): - brahminical_elites: Agenda-setter
 *   (institutional/identity_locked) — interprets, transmits, and adjudicates
 *   the corpus; collects ritual fees and deference - upper_caste_landholders:
 *   Primary beneficiary (powerful/constrained) — collects labor, rent, and
 *   deference warranted by the texts without running the apparatus -
 *   dalit_and_outcaste_communities: Primary target (powerless/trapped) —
 *   placed outside the order, assigned polluting labor, punished at the
 *   boundary - shudra_laboring_castes: Target (powerless/trapped) — born into
 *   service obligation, barred from study and office -
 *   women_under_patrilineal_prescriptions: Cross-cutting target
 *   (powerless/identity_locked) — bound at every rank by the patrilineal
 *   prescriptions - abolitionist_tradition_intellectuals: Organized dissent,
 *   excluded from the corpus's own adjudicative seats (organized/constrained)
 *   - comparative_dharmasastra_scholars: Analytical observer
 *   (analytical/analytical) — reconstructs prescription versus practice
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, 0.86).
domain_priors:suppression_score(dharmasastra_corpus__abolitionist_rejection, 0.72).
domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, extractiveness, 0.86).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0.54).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Dharmasastra Corpus as Operative Normative Authority (Abolitionist Rejection Reading)").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__abolitionist_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, '1bf4cb94-8513-497e-b1eb-21c55ac986a3').
narrative_ontology:cs_kernel_codification('1bf4cb94-8513-497e-b1eb-21c55ac986a3', fixed_text).
narrative_ontology:cs_authority_grounding('1bf4cb94-8513-497e-b1eb-21c55ac986a3', lineage).
narrative_ontology:cs_interpretation_layer_present('1bf4cb94-8513-497e-b1eb-21c55ac986a3').
narrative_ontology:cs_reading_relation('1bf4cb94-8513-497e-b1eb-21c55ac986a3', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('1bf4cb94-8513-497e-b1eb-21c55ac986a3', dharmasastra_corpus__reformist_contextual, coexists_with).
narrative_ontology:cs_axiom('1bf4cb94-8513-497e-b1eb-21c55ac986a3', foundational, corpus_authority_wholly_illegitimate).
narrative_ontology:cs_axiom_status(corpus_authority_wholly_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('1bf4cb94-8513-497e-b1eb-21c55ac986a3', corpus_authority_wholly_illegitimate, deontological).
narrative_ontology:cs_axiom('1bf4cb94-8513-497e-b1eb-21c55ac986a3', foundational, caste_hierarchy_dismantled_not_reinterpreted).
narrative_ontology:cs_axiom_status(caste_hierarchy_dismantled_not_reinterpreted, holdable).
narrative_ontology:cs_axiom_grounding('1bf4cb94-8513-497e-b1eb-21c55ac986a3', caste_hierarchy_dismantled_not_reinterpreted, deontological).
narrative_ontology:cs_reference_frame('1bf4cb94-8513-497e-b1eb-21c55ac986a3', corpus_as_dominative_construction).
narrative_ontology:cs_drift_state('1bf4cb94-8513-497e-b1eb-21c55ac986a3', post_ambedkarite_constitutional_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('1bf4cb94-8513-497e-b1eb-21c55ac986a3', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, brahminical_elites).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, upper_caste_landholders).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, dalit_and_outcaste_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, shudra_laboring_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, women_under_patrilineal_prescriptions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, abolitionist_tradition_intellectuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret, teach, and transmit the corpus; staff ritual offices; adjudicate disputes about duty, purity, and inheritance; collect dakshina, dana, and service payments tied to ritual indispensability. Their rank at the apex of the order is constituted by the texts they administer; renouncing the framework would dissolve the status, livelihood, and self-understanding built on it, so departure is not a realistic option even for the privately doubting.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, brahminical_elites, agenda_setter,
    institutional, generational, identity_locked, continental).

% Hold land, patronage ties, and local dominance legitimated by corpus-endorsed allocations of duty; receive labor, tribute, and deference from cultivating and servile castes. They do not run the interpretive apparatus but depend on its warrants. Leaving would mean surrendering rent-bearing legitimacy; most accommodate reform waves rather than exit.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, upper_caste_landholders, beneficiary,
    powerful, generational, constrained, regional).

% Born into service obligations owed to the twice-born; barred from Vedic study and from most ritual offices; bear corvee and household-service expectations enforced by custom and community sanction. Marriage and occupation are bounded at birth; moving villages changes masters, not station.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, shudra_laboring_castes, payer,
    powerless, generational, trapped, continental).

% Placed outside the varna order altogether as untouchables; assigned polluting labor such as scavenging, leatherwork, and corpse handling; subjected to residential segregation, temple and well entry denial, and violence when boundaries are crossed. Coordinated refusal — conversion movements, assertion politics, labor withdrawal — has been the main lever of change, and the record shows it answered with massacres and reprisals.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dalit_and_outcaste_communities, payer,
    powerless, generational, trapped, continental).

% Across every rank, the corpus prescribes obedience to father, husband, and son; bars women from Vedic recitation; ties religious efficacy to wifely devotion; restricts property and remarriage. The prescriptions bind regardless of a woman's own caste position, and enforcement runs through family honor and marriageability rather than through any court.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, women_under_patrilineal_prescriptions, payer,
    powerless, biographical, identity_locked, continental).

% Organized currents running from Phule through Ambedkar to Periyar and their successors: they deny the corpus any normative warrant, campaign for annihilation of caste rather than reform of it, and build counter-institutions — schools, presses, conversion movements, parties. Inside the corpus's own adjudicative seats they have never held a place; their leverage comes from mass mobilization and, latterly, constitutional law. Many are themselves of the communities the order subordinates.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, abolitionist_tradition_intellectuals, excluded,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__abolitionist_rejection, abolitionist_tradition_intellectuals, payer).

% Historians and philologists of the tradition reconstruct what the texts prescribed, how kings and courts actually used them, and where practice diverged from prescription. They hold no stake in observance or rejection and publish the divergences that both camps prefer to flatten.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, comparative_dharmasastra_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__abolitionist_rejection, brahminical_elites).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__abolitionist_rejection, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The corpus coordinated a stratified agrarian civilization: it allocated occupational and ritual duties across hereditary groups, regulated marriage and inheritance, gave kings a legible code for adjudication, and supplied a shared normative vocabulary linking scattered polities and generations.
% TRANSFER_FUNCTION: Moves labor, service, ritual fees, deference, and marriage-alliance control upward — from servile and outcaste castes and from women to brahminical elites and upper-caste patrons — and concentrates interpretive authority in trained pandit lineages.
% ABSENT_VOICES: The subordinated themselves: no Dalit, Shudra, or woman sat in the commentarial or court-advisory seats that produced the corpus's rulings; unanimity about duty was manufactured by excluding everyone bound by it. Their descendants now speak through assertion movements and constitutional politics, outside the corpus's frame entirely.
% DISAPPEARANCE_RATIONALE: Marriage networks, ritual economies, temple patronage, inheritance custom, and village labor arrangements currently route through or cite the corpus's warrants; overnight removal would force immediate renegotiation of who may marry whom, who performs which rites, who settles disputes, and on what warrant — a continent-scale rearrangement, even though much of the underlying social practice would immediately begin seeking new carriers.
% FOUNDING_PROBLEM: How to give a large, heterogeneous, largely illiterate agrarian society a stable, transmissible code of duty: who does what work, who marries whom, how disputes are settled, how ritual continuity is maintained across succession and conquest.
% FOUNDING_PROBLEM_CORROBORATION: Descriptive corroboration from outside the beneficiary set: historians of South Asian law (Lingat, Rocher, Olivelle and their successors) attest the corpus's founding role in royal adjudication and social ordering — and equally attest that modern states now perform those functions through enacted law. Normative corroboration that the mandate is void comes from the subordinated themselves — Ambedkar's Annihilation of Caste and the assertion movements — who testify that the ordering was always the mechanism of their subordination. No party outside the beneficiary set attests the mandate as live.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__abolitionist_rejection, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__abolitionist_rejection, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__abolitionist_rejection, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dharmasastra_corpus__abolitionist_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__abolitionist_rejection, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__abolitionist_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__abolitionist_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is authored from this reading's own lights: the corpus-as-authority is a domination machinery whose coordination story (cosmic order, social stability) functions as cover, whose persistence depends on enforcement and exit-suppression, and whose victims are identifiable — snare. Metrics are authored independently as descriptions of the arrangement's operation. Extractiveness 0.86: the transfer moves labor, fees, deference, and life-chances across a continental population graded at birth. Suppression 0.72: formal legal enforcement was abolished in the mid-twentieth century, but identity-lock at birth, community sanction, marriage control, and episodic violence still close exits — hence the dip-and-recovery shape in the suppression series. Theater 0.54 and rising: with kings gone and courts secular, a growing share of corpus-related activity is identity performance and citation rather than operative ordering. Accessibility_collapse 0.62: exits exist (conversion, urban anonymity, secular law) but cost kinship, community, and often safety. Resistance 0.78: two centuries of organized anti-caste movements, mass conversion, and constitutional repudiation. Identity-lock does different work at the two poles: elites are locked by constitutive status (exit dissolves the self and the livelihood together), the subordinated by hereditary placement (exit is punished before it can be chosen). Coalition power is the recorded counterweight: the payer seats are numerous, and their coalitions — the Satyashodhak Samaj, the Self-Respect Movement, Dalit assertion parties — are the only force that has ever moved the arrangement. The three measurement series share one seven-point grid; every tracked metric is authored at every point, so no end-state value is silently substituted into earlier times.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural data. From the brahminical elite seat the arrangement presents as a received, sacred order that it administers at real personal cost of discipline and ritual burden — the orthodox sibling reading is that seat's honest report. From the Dalit seat the same structure presents as totalizing extraction with no exit — this story's report. The reformist seat straddles: it concedes the subordination it observes from a position the order protects. The engine derives these per-seat classifications from power, exit, and role; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d toward zero: brahminical_elites (fees, interpretive monopoly, status apex — subsidized, and identity-locked so the subsidy is durable) and upper_caste_landholders (rent and deference warranted by the texts). Victim declarations drive d toward one: dalit_and_outcaste_communities and shudra_laboring_castes are trapped — hereditary placement, punished exit — and sit nearest the full-target end; women_under_patrilineal_prescriptions are cross-cutting targets bound at every rank. Abolitionist intellectuals have exercised normative exit while remaining socially embedded, damping their d below the trapped seats. Scholars are analytical and feed no directionality. Continental spatial scope scales effective extraction upward for the target seats: verification of boundary violence is hardest exactly where the affected population is largest.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading drives the founding-problem status to dead while the disappearance verdict stays world_rearranges — the exact mismatch the R5 consumer flags as zombie/capture: an arrangement whose warrant is void but whose world still organizes around it. The classification work here is preventive in two directions. Against the reformist rescue: salvaging an ethical core would re-found a coordination function and re-enter the corpus as a hybrid arrangement; the abolitionist refusal of salvage keeps the extraction visible instead of laundering it through reinterpretation. Against a piton misread: theater_ratio is elevated and rising, but the gains concentrate in a named seat (brahminical_elites) and fixing remains prohibitive — capture, not neglect, holds the arrangement up, which is the signature of a snare, not an inertial leftover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_index_epsilon_divergence,
    'This story instantiates the abolitionist_rejection reading of kernel dharmasastra_corpus; the same corpus-referent carries different authored epsilon under the orthodox_literalist and reformist_contextual readings — where exactly is the disagreement located?',
    'Compare the three reading-stories'' epsilon values, victim sets, and axioms; locate the divergence at the authority premise (whether any corpus content legitimately binds) rather than at empirical disagreement about the historical record.',
    'Resolution toward the reformist premise shrinks the victim set to caste-prescription harms and reshapes this story toward a salvage-with-residue structure; resolution toward the orthodox premise inverts the beneficiary/victim structure entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_index_epsilon_divergence, conceptual, 'Committer structure: one reading of a contested kernel; epsilon is reading-indexed over a fixed referent.').

omega_variable(
    constructed_vs_ordained_hierarchy,
    'Is the varna/jati hierarchy a constructed arrangement serving identifiable beneficiaries, as this reading holds, or a divinely ordained order whose differential duties are intrinsic, as the orthodox reading holds?',
    'Not resolvable by data within any single framework — the orthodox premise is theological, so resolution happens at the level of framework adoption. Historical evidence (jati fluidity, regional variation, the politics of codification) constrains the constructed reading''s details without settling the origin question.',
    'Under the ordained reading, beneficiaries vanish (no one merely benefits from dharma) and this story''s entire structural declaration collapses; this reading bets the reverse and authors beneficiaries accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_ordained_hierarchy, conceptual, 'Origin contest over the hierarchy: constructed domination versus ordained order, documented from the abolitionist side.').

omega_variable(
    textual_cause_vs_social_carrier,
    'Does the corpus cause the caste order it describes, or does it merely codify and legitimate a jati order carried by kinship, marriage, and economic practice that would persist substantially without the texts?',
    'Comparative-historical analysis: caste dynamics in regions and periods with thin shastric penetration; persistence trajectories after textual authority collapsed under colonial courts and constitutional repudiation.',
    'If caste is primarily socially carried, abolishing the textual framework eliminates less than this reading claims — the victim set persists under a non-textual carrier and the abolitionist remedy is insufficient rather than wrong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_cause_vs_social_carrier, empirical, 'Causal weight of the texts versus the social practice they encode.').

omega_variable(
    decentralized_enforcement_locus,
    'After constitutional repudiation removed formal enforcement, is the arrangement''s continued operation carried by decentralized community enforcement (family, caste council, marriage market) that no longer cites the corpus?',
    'Sociological measurement of enforcement episodes: whether actors invoke shastric warrant or purely customary and social sanction; systematic tracking of honor-violence and caste-council enforcement cases.',
    'If enforcement no longer routes through the corpus, the standing arrangement''s suppression is increasingly carried by successor structures, and this constraint''s effective suppression declines even as lived subordination persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_enforcement_locus, empirical, 'Where enforcement actually lives after formal repudiation.').

omega_variable(
    intermediate_caste_dual_position,
    'Intermediate castes sit below twice-born extraction and above outcaste exclusion simultaneously — do they belong in the victim set, the beneficiary set, or both?',
    'Positional analysis by specific jati: net flow of payments, labor obligation, ritual status, and marriage control across the gradient, rather than a two-block tally.',
    'Placing them in both sets moderates the reading''s binary victim/beneficiary structure; placing them as net victims sharpens it. Either way the hierarchy''s gradient, not a simple split, carries the transfers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intermediate_caste_dual_position, conceptual, 'Gradient position of intermediate castes complicates the binary structural declaration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0, 0.08).
narrative_ontology:measurement(dhar_tr_t4, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 4, 0.11).
narrative_ontology:measurement(dhar_tr_t8, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 8, 0.16).
narrative_ontology:measurement(dhar_tr_t12, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 12, 0.28).
narrative_ontology:measurement(dhar_tr_t16, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 16, 0.4).
narrative_ontology:measurement(dhar_tr_t20, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 20, 0.48).
narrative_ontology:measurement(dhar_tr_t24, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 24, 0.54).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 0, 0.86).
narrative_ontology:measurement(dhar_be_t4, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 4, 0.89).
narrative_ontology:measurement(dhar_be_t8, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 8, 0.91).
narrative_ontology:measurement(dhar_be_t12, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 12, 0.84).
narrative_ontology:measurement(dhar_be_t16, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 16, 0.79).
narrative_ontology:measurement(dhar_be_t20, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 20, 0.81).
narrative_ontology:measurement(dhar_be_t24, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 24, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(dhar_su_t4, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 4, 0.78).
narrative_ontology:measurement(dhar_su_t8, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 8, 0.83).
narrative_ontology:measurement(dhar_su_t12, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 12, 0.76).
narrative_ontology:measurement(dhar_su_t16, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(dhar_su_t20, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(dhar_su_t24, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__abolitionist_rejection, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__reformist_contextual).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Dharmasastra' decomposes into three reading-stories of one kernel sharing a single referent (the corpus as operative normative authority) with reading-indexed epsilon. Upstream/downstream: the orthodox_literalist story (lowest epsilon, claims of eternity) supplies the legitimacy warrant the other two respond to; the reformist_contextual story mediates; this abolitionist story terminates the family by denying the warrant outright. Each story links the other two through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
