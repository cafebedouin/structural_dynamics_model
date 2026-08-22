% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__indigenous_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__indigenous_stewardship_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__indigenous_stewardship_reading
 *   human_readable: Cultural Property Retention Regime (Indigenous Stewardship Reading)
 *   domain: law/international/cultural_property_post_colonial
 *
 * SUMMARY:
 *   The standing arrangement under contest: indigenous sacred and communal
 *   objects removed during colonial administration are held, as a matter of
 *   settled law and institutional policy, by encyclopedic museums and by
 *   successor states that inherited colonial collections through national
 *   patrimony statutes. Custody is protected by export controls,
 *   inalienability and anti-deaccession rules, limitation periods, and claim
 *   procedures whose evidentiary standards the holders themselves administer.
 *   This file instantiates ONE reading of the cultural_property_legal_corpus
 *   kernel: the indigenous_stewardship_reading, under which legitimate
 *   authority rests with communities maintaining cultural continuity, and
 *   neither successor states nor museums hold legitimate claim. Per the
 *   epsilon-referent rule, extractiveness is authored for the standing
 *   arrangement AS THIS READING ASSESSES IT - hence the highest epsilon in
 *   the constraint family - and not for the community-custody arrangement
 *   this reading endorses. The sibling readings are separate constraint files
 *   linked through network.affects_constraints. Claim and metrics are
 *   independent authored facts: the claimed type records this reading's
 *   structural verdict (tangled_rope: real conservation coordination plus
 *   enforced asymmetric extraction); the metrics record the arrangement's
 *   operation as this reading observes it.
 *
 * KEY AGENTS:
 *   - - indigenous_source_communities: primary target (organized/trapped) - bears dispossession costs; no lawful route to possession bypasses holder consent
 *   - - universal_museums: primary beneficiary and agenda-setter (institutional/arbitrage) - administers custody, sets claim standards, collects revenue and prestige
 *   - - colonial_successor_states: secondary beneficiary with enforcement role (institutional/arbitrage) - writes patrimony law, defends retention diplomatically
 *   - - cultural_heritage_industry: tertiary beneficiary (powerful/arbitrage) - profits from trade in provenance-ambiguous material
 *   - - ceremonial_knowledge_keepers: excluded voice (powerless/trapped) - ritual authority displaced, no seat in governing forums
 *   - - museum_going_publics: diffuse beneficiary with indirect costs (moderate/mobile)
 *   - - un_indigenous_rights_mechanisms: analytical observer (institutional/analytical) - documents the standards-practice gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.86).
domain_priors:suppression_score(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.64).
domain_priors:theater_ratio(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__indigenous_stewardship_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__indigenous_stewardship_reading, "Cultural Property Retention Regime (Indigenous Stewardship Reading)").
narrative_ontology:topic_domain(cultural_property_legal_corpus__indigenous_stewardship_reading, "law/international/cultural_property_post_colonial").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__indigenous_stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__indigenous_stewardship_reading, '078e26cc-6e49-4f06-9f4e-57f1b663b598').
narrative_ontology:cs_kernel_codification('078e26cc-6e49-4f06-9f4e-57f1b663b598', formalized).
narrative_ontology:cs_authority_grounding('078e26cc-6e49-4f06-9f4e-57f1b663b598', distributed).
narrative_ontology:cs_reading_relation('078e26cc-6e49-4f06-9f4e-57f1b663b598', cultural_property_legal_corpus__sovereign_repatriation_reading, forecloses).
narrative_ontology:cs_reading_relation('078e26cc-6e49-4f06-9f4e-57f1b663b598', cultural_property_legal_corpus__universal_heritage_reading, forecloses).
narrative_ontology:cs_axiom('078e26cc-6e49-4f06-9f4e-57f1b663b598', foundational, cultural_continuity_confers_custodial_authority).
narrative_ontology:cs_axiom_status(cultural_continuity_confers_custodial_authority, holdable).
narrative_ontology:cs_axiom_grounding('078e26cc-6e49-4f06-9f4e-57f1b663b598', cultural_continuity_confers_custodial_authority, deontological).
narrative_ontology:cs_axiom('078e26cc-6e49-4f06-9f4e-57f1b663b598', foundational, nonconsensual_removal_voids_transfer_of_authority).
narrative_ontology:cs_axiom_status(nonconsensual_removal_voids_transfer_of_authority, holdable).
narrative_ontology:cs_axiom_grounding('078e26cc-6e49-4f06-9f4e-57f1b663b598', nonconsensual_removal_voids_transfer_of_authority, deontological).
narrative_ontology:cs_reference_frame('078e26cc-6e49-4f06-9f4e-57f1b663b598', community_custodial_sovereignty).
narrative_ontology:cs_drift_state('078e26cc-6e49-4f06-9f4e-57f1b663b598', post_undrip_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('078e26cc-6e49-4f06-9f4e-57f1b663b598', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, universal_museums).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_heritage_industry).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_source_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, museum_going_publics).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, museum_going_publics).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, state_permanent_patrimony_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, museum_trusteeship_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, prospective_only_remedy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities whose ceremonial and communal objects were removed during colonial administration and now sit in overseas collections. They maintain the living practices the objects belong to, pursue return through negotiation, litigation, and international advocacy, and carry the ongoing costs: interrupted ceremony, gaps in knowledge transmission, and the burden of proving entitlement under procedures whose evidentiary standards the holding institutions themselves set. There is no route to lawful possession that bypasses holder consent.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_source_communities, payer,
    organized, generational, trapped, global).

% Encyclopedic museums holding large indigenous collections acquired during colonial periods. They set acquisition, loan, display, and deaccession policy, administer claim procedures, and determine what counts as adequate provenance. They draw visitor revenue, scholarly capital, and civic prestige from the collections, and can restructure their exposure through long-term loans, digital programs, and partnership agreements without surrendering title.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, universal_museums, agenda_setter,
    institutional, civilizational, arbitrage, global).

% States that inherited colonial-era holdings through national patrimony laws and state-funded museums. They legislate export controls and inalienability rules, defend retention in diplomatic fora, and draw tourism income and nation-branding value from the collections. They can soften exposure case-by-case through bilateral returns while leaving the general legal architecture intact.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states, agenda_setter).

% Auction houses, dealers, and private collectors operating in the licit and gray markets around provenance-ambiguous material. They profit from transaction fees, appraisals, and the scarcity premium that restricted supply creates, and can move capital and inventory across jurisdictions faster than any claim process runs.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_heritage_industry, beneficiary,
    powerful, immediate, arbitrage, global).

% Visitors and taxpayers who enjoy access to world collections and subsidize holding institutions through public funding. They bear diffuse costs: ticket and tax support for retention disputes, and the opportunity cost of access models that exclude origin communities. They can choose which institutions to visit but shape custody policy only as voters and donors.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, museum_going_publics, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, museum_going_publics, payer).

% Elders and ritual specialists whose practice depends on physical access to specific objects. They hold no seat in the state-to-state conventions and institutional claim procedures that govern the material; eligibility rules and documentary requirements are set by others. Where objects stay inaccessible, segments of ceremonial knowledge lapse with them.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, ceremonial_knowledge_keepers, excluded,
    powerless, generational, trapped, local).

% UN treaty bodies, the Expert Mechanism on the Rights of Indigenous Peoples, and special rapporteurs that document the gap between declared standards and custody practice, take testimony from communities and states alike, and issue recommendations that generate normative pressure but bind no collection.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, un_indigenous_rights_mechanisms, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__indigenous_stewardship_reading, universal_museums).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__indigenous_stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralized custody solves real problems once: conservation science, climate-controlled storage, emergency protection in armed conflict, cataloguing and provenance documentation, and broad public and scholarly access to material that would otherwise be dispersed and fragile.
% TRANSFER_FUNCTION: Moves custody, interpretive authority, ritual access, and economic returns (visitor revenue, licensing, prestige, nation-branding value) from originating communities to holding institutions and successor states; moves the normative cost of illegitimacy charges back onto the holders.
% ABSENT_VOICES: Ceremonial knowledge keepers and community governance bodies were absent when the governing instruments were drafted: the 1970 UNESCO Convention was negotiated state-to-state, and institutional claim procedures are designed by the holders. Present in the room, they would object that the regime converts living relationships between communities and their material into property questions between states and museums.
% DISAPPEARANCE_RATIONALE: If the retention architecture vanished overnight, custody, title, conservation funding flows, and access regimes would all reorganize around community authority: collections would disperse to origin communities, holding institutions would lose core galleries and the revenue and prestige attached to them, successor-state patrimony law would lose its object, and the trade in provenance-ambiguous material would reprice.
% FOUNDING_PROBLEM: Mid-century efforts to stop wartime looting and illicit trafficking in cultural material (the 1954 Hague protocols and the 1970 UNESCO Convention), joined to an older preservationist aim of protecting fragile material under professional care.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: indigenous delegations' drafting testimony for UNDRIP Articles 11-12, UN treaty-body concluding observations, and independent provenance scholarship documenting that the 1970 Convention was prospective-only and never governed the pre-1970 holdings that make up most contested material. No corroborating source attests that permanent retention by holders was the intended remedy; states and museums attest only the anti-trafficking rationale, which does not reach existing collections.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__indigenous_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__indigenous_stewardship_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__indigenous_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.86 at interval end) because the arrangement's benefit side (conservation, access, scholarship) is real but its cost side falls almost entirely on communities with no consensual entry point: the 1970 Convention was prospective-only, so the bulk of contested holdings were grandfathered into legitimacy, and each decade of retention compounds the gap between custody and authority. Suppression (0.64 scalar) reflects the structural architecture - limitation periods, burden of proof on claimants, anti-deaccession rules - which persists even though the suppression_requirement series shows active enforcement intensity rising through 2000 (export licensing, immunity-from-seizure statutes, inalienability doctrines maturing) and receding afterward as normative pressure made open coercion costly and voluntary frameworks substituted. Theater_ratio climbs steadily to 0.52: acknowledgment statements, contested-histories galleries, shared-stewardship language, and digital access programs increasingly substitute for title transfer, crossing the Goodhart threshold by interval end. Accessibility_collapse is low-moderate (0.38) because alternatives exist on paper (claims, negotiation, bilateral return) but rarely deliver custody; resistance is substantial (0.62) and rising, carried by litigation, UNDRIP implementation, and campaign pressure. All three series share one time grid (1970-2025, seven points) so no metric is sampled against another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute sharply different arrangements from identical structural data. From the community seat (payer, trapped, generational horizon) the arrangement is dispossession administered as procedure - extraction with no exit. From the museum seat (agenda-setter, arbitrage, civilizational horizon) the same structure is trusteeship under siege: duties held against illegitimate demands. From the successor-state seat (beneficiary with enforcement powers) it is sovereignty defense. Museum-going publics sit near symmetric: genuine access benefit, diffuse subsidy and growing unease. The excluded keepers' seat, absent from every governing forum, experiences the purest form of the arrangement - authority exercised over their practice by strangers. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: universal_museums collect custody revenues and prestige directly; colonial_successor_states collect patrimony legitimacy and tourism income while writing the enforcing law; the heritage industry collects transaction rents. Victims derive high directionality, amplified by trapped exit - indigenous_source_communities cannot route around holder consent, so their effective extraction sits near the full-target end. Global spatial scope modestly amplifies effective extraction for the paying seat because verification of claim circumstances crosses jurisdictions the holders dominate. Ceremonial_knowledge_keepers are not separately declared in the victim arrays (subsumed under indigenous_source_communities) but are structurally nearest the full-target end; the excluded seat is recorded, not scored. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the correct d for every seated agent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stopping wartime looting and illicit trafficking) is partially live - the illicit market persists - so founding_problem_status is contested rather than dead, and the dead-plus-world_rearranges zombie flag does not fire. But the theater series shows the retention function drifting away from its founding justification: enforcement machinery built for trafficking now guards holdings the trafficking conventions never reached. The tangled_rope classification prevents two opposite mislabelings. Calling the arrangement a snare would erase the genuine conservation and access goods that make the coordination face real and would deny the publics' and scholars' stake; calling it a rope would erase the enforced asymmetry that leaves communities outside their own material's authority. The forward risk is mandatrophy in the classic direction: if the illicit-trade problem is closed while retention persists on theatrical stewardship claims, the arrangement completes the drift toward performance maintained by inertia - visible already in theater_ratio crossing 0.5.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_authority_locus_dispute,
    'This story instantiates the indigenous_stewardship_reading of kernel cultural_property_legal_corpus; the sibling readings (sovereign_repatriation_reading, universal_heritage_reading) place legitimate authority in successor states or in access-maximizing institutions instead. Which authority locus governs classification, and what happens to epsilon and the beneficiary/victim structure under each sibling?',
    'Binding adjudication of the authority question: treaty recognition of community legal personality over material heritage, or a tribunal ruling on whose continuity claim grounds custodial authority. Until then the three readings remain separate constraint files linked by network edges, each with its own epsilon.',
    'Under sovereign_repatriation_reading the victim set shifts (communities are subsumed into state continuity claims) and epsilon falls below this reading''s value; under universal_heritage_reading the arrangement''s extraction collapses toward coordination cost and community-standing remedies dissolve. This story''s classification is valid only within this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_authority_locus_dispute, conceptual, 'Committer-frame omega: one kernel, three mutually exclusive authority loci; this file authors only the community-locus reading.').

omega_variable(
    continuity_verification_gatekeeping,
    'Who verifies that a claimant community maintains the cultural continuity this reading requires, and can the verification standard become a colonial-style authentication burden imposed on the very people the reading empowers?',
    'Compare repatriation practice under community-defined criteria (UNDRIP self-identification principle) against institution-imposed evidentiary thresholds: grant rates, procedural costs, and who bears the burden of proof.',
    'Strict external verification reproduces the dispossession dynamic inside the remedy and raises measured suppression; community-defined criteria make the authority transfer substantive rather than nominal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_verification_gatekeeping, empirical, 'Whether the reading''s continuity test functions as empowerment or as gatekeeping.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the limited volume of community claims structural (limitation periods, burden-of-proof rules, anti-deaccession statutes) or internalized (absorption of the ''objects are safer and better cared for elsewhere'' narrative propagated by holding institutions)?',
    'Post-return trajectories: if returned objects re-enter active ceremonial use and claim volumes rise where procedures simplify, prior quiescence was structural; if returned material remains sequestered and demand stays flat, internalization runs deeper than the legal record shows.',
    'An internalized component means effective suppression exceeds the structural measure and persists after legal barriers fall; purely structural suppression declines with reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression behind claim quiescence.').

omega_variable(
    retention_gain_capture_concentration,
    'Do the gains of retention concentrate in identifiable seats (trustee bodies, host-city economies, state treasuries) or diffuse across visiting publics and the scholarly commons?',
    'Exhibition-economics disclosure, tourism-revenue attribution studies, and analysis of who funds and who profits from flagship contested collections.',
    'Concentrated capture pushes the retention function toward snare structure with identifiable rent collectors; diffuse capture keeps it a hybrid with genuine public-good spillovers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retention_gain_capture_concentration, empirical, 'Where the gains of retention actually land.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__indigenous_stewardship_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t1970, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement_basis(cult_tr_t1970, observed).
narrative_ontology:measurement(cult_tr_t1980, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 1980, 0.17).
narrative_ontology:measurement_basis(cult_tr_t1980, observed).
narrative_ontology:measurement(cult_tr_t1990, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement_basis(cult_tr_t1990, observed).
narrative_ontology:measurement(cult_tr_t2000, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement_basis(cult_tr_t2000, observed).
narrative_ontology:measurement(cult_tr_t2010, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 2010, 0.41).
narrative_ontology:measurement_basis(cult_tr_t2010, observed).
narrative_ontology:measurement(cult_tr_t2018, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 2018, 0.47).
narrative_ontology:measurement_basis(cult_tr_t2018, observed).
narrative_ontology:measurement(cult_tr_t2025, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 2025, 0.52).
narrative_ontology:measurement_basis(cult_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(cult_be_t1970, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 1970, 0.74).
narrative_ontology:measurement_basis(cult_be_t1970, observed).
narrative_ontology:measurement(cult_be_t1980, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 1980, 0.77).
narrative_ontology:measurement_basis(cult_be_t1980, observed).
narrative_ontology:measurement(cult_be_t1990, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement_basis(cult_be_t1990, observed).
narrative_ontology:measurement(cult_be_t2000, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement_basis(cult_be_t2000, observed).
narrative_ontology:measurement(cult_be_t2010, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement_basis(cult_be_t2010, observed).
narrative_ontology:measurement(cult_be_t2018, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 2018, 0.85).
narrative_ontology:measurement_basis(cult_be_t2018, observed).
narrative_ontology:measurement(cult_be_t2025, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 2025, 0.86).
narrative_ontology:measurement_basis(cult_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t1970, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement_basis(cult_su_t1970, observed).
narrative_ontology:measurement(cult_su_t1980, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 1980, 0.63).
narrative_ontology:measurement_basis(cult_su_t1980, observed).
narrative_ontology:measurement(cult_su_t1990, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement_basis(cult_su_t1990, observed).
narrative_ontology:measurement(cult_su_t2000, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement_basis(cult_su_t2000, observed).
narrative_ontology:measurement(cult_su_t2010, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement_basis(cult_su_t2010, observed).
narrative_ontology:measurement(cult_su_t2018, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 2018, 0.62).
narrative_ontology:measurement_basis(cult_su_t2018, observed).
narrative_ontology:measurement(cult_su_t2025, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 2025, 0.55).
narrative_ontology:measurement_basis(cult_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__indigenous_stewardship_reading, resource_allocation).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'who owns culture?' decomposes into three structurally distinct readings of the cultural_property_legal_corpus kernel, each with its own epsilon, beneficiary/victim structure, and classification. The universal_heritage_reading is the established upstream baseline (mid-century consensus) whose preservation-and-access claims both challenger readings cite or attack; the indigenous_stewardship_reading (this file) and the sovereign_repatriation_reading compete for the challenger position, differing on the locus of legitimate authority. This file authors the community-locus reading only; epsilon is highest here because the standing arrangement vests custody in parties this reading holds to have no legitimate claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
