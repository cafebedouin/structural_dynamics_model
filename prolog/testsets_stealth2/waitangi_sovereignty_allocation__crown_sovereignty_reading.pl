% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__crown_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__crown_sovereignty_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__crown_sovereignty_reading
 *   human_readable: Crown Sovereignty Reading of the Waitangi Article I Cession (Westminster Parliamentary Supremacy)
 *   domain: constitutional/post-colonial governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. Kernel: the
 *   sovereignty clause of the Treaty of Waitangi (Article I), a fixed-text
 *   commitment read differently by different parties. This reading —
 *   crown_sovereignty_reading — holds that the English text's cession of
 *   'complete sovereignty' is the authoritative record, grounding Westminster
 *   parliamentary supremacy: the Crown legislates for New Zealand without a
 *   Maori consent requirement, allocates resources unilaterally, and
 *   subordinates Maori interests to parliamentary will. The sibling readings
 *   (partnership_reading, rangatiratanga_reading) are separate constraint
 *   stories with their own epsilon, victim sets, and classifications; nothing
 *   about them is averaged into this file. The epsilon referent is the
 *   standing arrangement under contest — plenary parliamentary supremacy as
 *   actually practiced 1840–present — assessed by this reading's own lights:
 *   the reading holds the cession valid while acknowledging that the
 *   historical operation of the arrangement produced documented, partially
 *   remediated harms it attributes to specific breaches rather than to the
 *   cession itself. Claim and metrics are authored independently: the claimed
 *   type is what I believe structurally true; the metrics describe the
 *   arrangement's actual operation. KEY AGENTS (by structural relationship):
 *   - crown_executive_parliament: Agenda-setting seat
 *   (institutional/arbitrage) — administers the ceded authority and collects
 *   its revenues - colonial_settler_population: Primary beneficiary
 *   (organized/mobile) — received land, franchise, and self-government under
 *   the arrangement - maori_iwi_hapu: Primary target
 *   (moderate/identity_locked) — bears the laws and the land losses without a
 *   consent step - non_signatory_rangatira: Excluded voice (moderate/trapped)
 *   — bound by a cession they never made - waitangi_tribunal: Analytical
 *   observer (institutional/analytical) — compiles the grievance record all
 *   seats now argue from
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.58).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.38).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Crown Sovereignty Reading of the Waitangi Article I Cession (Westminster Parliamentary Supremacy)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional/post-colonial governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'b3006dbc-e8dc-4a88-bb66-fcd4977be9f4').
narrative_ontology:cs_kernel_codification('b3006dbc-e8dc-4a88-bb66-fcd4977be9f4', fixed_text).
narrative_ontology:cs_authority_grounding('b3006dbc-e8dc-4a88-bb66-fcd4977be9f4', lineage).
narrative_ontology:cs_interpretation_layer_present('b3006dbc-e8dc-4a88-bb66-fcd4977be9f4').
narrative_ontology:cs_reading_relation('b3006dbc-e8dc-4a88-bb66-fcd4977be9f4', waitangi_sovereignty_allocation__partnership_reading, influences).
narrative_ontology:cs_reading_relation('b3006dbc-e8dc-4a88-bb66-fcd4977be9f4', waitangi_sovereignty_allocation__rangatiratanga_reading, forecloses).
narrative_ontology:cs_axiom('b3006dbc-e8dc-4a88-bb66-fcd4977be9f4', foundational, english_text_sovereignty_cession_authoritative).
narrative_ontology:cs_axiom_status(english_text_sovereignty_cession_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('b3006dbc-e8dc-4a88-bb66-fcd4977be9f4', english_text_sovereignty_cession_authoritative, conventional).
narrative_ontology:cs_axiom('b3006dbc-e8dc-4a88-bb66-fcd4977be9f4', secondary, parliament_may_legislate_without_maori_consent).
narrative_ontology:cs_axiom_status(parliament_may_legislate_without_maori_consent, holdable).
narrative_ontology:cs_axiom_grounding('b3006dbc-e8dc-4a88-bb66-fcd4977be9f4', parliament_may_legislate_without_maori_consent, conventional).
narrative_ontology:cs_reference_frame('b3006dbc-e8dc-4a88-bb66-fcd4977be9f4', absolute_cession_westminster_supremacy).
narrative_ontology:cs_drift_state('b3006dbc-e8dc-4a88-bb66-fcd4977be9f4', post_waitangi_tribunal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b3006dbc-e8dc-4a88-bb66-fcd4977be9f4', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_executive_parliament).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, colonial_settler_population).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, non_signatory_rangatira).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the legislative authority this reading says was ceded in 1840. Writes and passes statutes for all of New Zealand without a Maori consent step, allocates Crown land and resources, and collects revenue from land sales and resource royalties. As the body that both makes and amends the constitutional rules, it faces no external forum it cannot reshape; its practical limits are political rather than legal.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_executive_parliament, agenda_setter,
    institutional, generational, arbitrage, national).

% Arrived under immigration schemes promising land and self-government. Received surveyed farmland, towns, and public works financed substantially from Crown land purchasing in Maori districts, and voted in a settler-franchise parliament until Maori seats were created in 1867. Individuals dissatisfied with conditions could return to Britain or move on to Australia; many did.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, colonial_settler_population, beneficiary,
    organized, biographical, mobile, national).

% Signed or declined the 1840 agreements, then lived thereafter under laws passed without their collective assent. Lost the great bulk of their land base through purchase, the Native Land Court's individualisation of title, and wartime confiscation; population fell steeply across the nineteenth century. Whakapapa ties them to particular territories — land is ancestor, not asset — so relocating off the whenua is not a real option; remaining means operating inside the imposed legal order. From the 1970s onward they have pursued claims, settlements, and co-governance arrangements through whatever institutions were open to them.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu, payer,
    moderate, generational, identity_locked, national).

% Chiefs who never signed at Waitangi or on the later sheets — among them communities in Taranaki, Waikato, and parts of the South Island — and rangatira who had asserted independent authority in He Whakaputanga o te Rangatiratanga o Nu Tireni (1835). Their authority was nonetheless treated as ceded once the Crown proclaimed sovereignty over the islands. They bear the same laws and land administration as signatories while having never been part of the assenting conversation; their objection survives in oral tradition, tribunal evidence, and whakapapa rather than in the constitutional text.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, non_signatory_rangatira, payer,
    moderate, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__crown_sovereignty_reading, non_signatory_rangatira, excluded).

% Statutory commission established in 1975, with jurisdiction backdated to 1840 in 1985, that hears claims that Crown actions broke Treaty promises and reports recommendations. It cannot bind Parliament; governments adopt, adapt, or set aside its findings. It compiles the evidentiary record — oral testimony, archives, commissioned history — on which every seat's account of the arrangement now draws.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_tribunal, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_executive_parliament).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__crown_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single unified legal and governmental order across the islands: common law, courts, currency, defence against rival imperial powers, and a framework replacing inter-iwi diplomacy for relations with the incoming settler population.
% TRANSFER_FUNCTION: Moves legislative authority and resource-allocation power from Maori polities to the Crown; subsequently moves land title from Maori collectives to individual grantees and settlers through Crown purchasing and the Native Land Court, and moves decision rights over forests, waterways, and minerals from hapu to Parliament.
% ABSENT_VOICES: Chiefs who never signed; the rangatira of the 1835 He Whakaputanga confederation who had declared independence five years earlier; and the many signatories who understood only the Maori text, in which Article I conveys kawanatanga (governorship) rather than 'sovereignty'. They sit outside the constitutional conversation because this reading treats the English text as the dispositive record of what was agreed.
% DISAPPEARANCE_RATIONALE: If plenary parliamentary supremacy vanished overnight, every statute, court, property title, and local authority in New Zealand would lose its stated legitimacy source; the constitutional order would have to be renegotiated into some co-governance or Treaty-grounded form before ordinary government could resume. Nothing about the current arrangement is self-maintaining without this foundation.
% FOUNDING_PROBLEM: By 1840 the islands had substantial unregulated settlement, lawlessness at Kororareka, French and American interest in annexation, and a humanitarian campaign in Britain against private land-sharks stripping Maori of land. The arrangement was built to solve orderly colonisation under a single authority while, on the British side's own stated terms, protecting Maori from predatory private purchase.
% FOUNDING_PROBLEM_CORROBORATION: Lord Normanby's 1839 instructions to Hobson and the Colonial Office correspondence corroborate the founding problem from outside the benefiting parties — annexation was justified in London expressly as protection of Maori from speculators. Historians (Claudia Orange, James Belich) corroborate both the problem and the gap between the protection promised and the confiscation delivered. Maori oral accounts corroborate a related but distinct problem — seeking a governor to control land-sharks and secure trade — which supports the kawanatanga rather than the cession understanding. No source outside the benefiting parties attests that the problem was resolved by complete cession specifically; that resolution is attested only by the Crown's own records.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__crown_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__crown_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58 sits mid-high: the arrangement performs a real governance function everyone now inhabits, yet its historical operation transferred nearly the whole Maori land base and all legislative authority without consent, and the partial remediation (settlements, co-governance) leaves the underlying authority structure untouched. Suppression 0.38 describes the current state — overt military suppression ended a century ago, replaced by procedural absorption — and is authored as a raw structural property, unscaled by power or scope; only extractiveness is scaled downstream. Theater_ratio 0.33: consultation exercises without consent effect and anniversary rhetoric carry performative weight, but courts, settlements, and service delivery are functional. Accessibility_collapse 0.62: Maori cannot exit the legal order and rival constitutional forms were historically suppressed, yet the kernel contest keeps alternative readings institutionally alive. Resistance 0.72: sustained and documented across the whole interval — the Kingitanga, Pai Marire, Parihaka's passive resistance, the 1975 land march, Bastion Point, Waitangi Day protests, and the 2024 hikoi moho te Tiriti. All three temporal series run on one shared nine-point grid (1840–2025) so every metric is authored at every examined time point. The series trace an enforcement-driven cycle rather than monotonic drift: war-era suppression peaks in the 1860s–70s alongside confiscation-driven extractiveness, an administrative plateau follows, tribunal-era recognition pulls extractiveness and suppression down from 1975, and the renewed constitutional contest (foreshore and seabed 2004, the Treaty Principles Bill episode 2024–25) produces the terminal uptick in all three series. The oscillation tracks enforcement intensity against Maori political mobilisation; it is a side effect of contest cycles, not itself an intermittent-reinforcement mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat the arrangement is lawful self-government inherited through a valid cession — the constraint it experiences is the ordinary burden of governing. From the settler seat it was opportunity: land, franchise, and public works financed from the transferred estate. From the payer seat the same structure operates as dispossession administered by procedure — laws passed over them, title individualised to make land purchasable, confiscation ratified by act. The excluded seat experiences a further layer: bearing costs of an assent event they never joined. The engine computes these divergent per-seat types from the structural data; this file's claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. crown_executive_parliament is declared beneficiary and holds arbitrage-grade exit (it writes the rules it lives under), placing it near the beneficiary pole — effective extraction damped toward subsidy. colonial_settler_population is beneficiary with mobile exit, similarly damped. maori_iwi_hapu is declared victim with identity_locked exit — whakapapa fuses the people to the whenua, so exit is not merely blocked but unthinkable without dissolving what exits — pushing d toward the full-target end and amplifying effective extraction. non_signatory_rangatira share the victim position with trapped exit and the added exclusion from the founding assent. waitangi_tribunal is an analytical seat, neither collecting nor paying. No directionality overrides were needed: the beneficiary/victim declarations plus exit options already produce the correct structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents mislabeling in both directions. Calling this pure coordination (rope) would erase the victims: the same structure that provided a unified legal order also carried confiscation, title individualisation, and consent-free legislation, and it required active enforcement — war, then law — to hold. Calling it pure extraction (snare) would erase the functioning legal order every party, including claimants, now argues inside, and would misread the founding problem, which was real and externally corroborated. On mandatrophy: the founding problem (orderly governance plus protection) is contested rather than dead — the governance half persists, the protection half inverted into the breach record the Tribunal documents. Because status is contested rather than dead, the dead-mandate mismatch flag should not fire; the arrangement is not a piton candidate — its function is real, its enforcement is real, and its beneficiaries are concentrated enough to maintain it deliberately, as the recent reassertion episode shows.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the crown_sovereignty_reading of the waitangi_sovereignty_allocation kernel; how would adopting a sibling reading change the structural data?',
    'Read this story against its siblings (partnership_reading, rangatiratanga_reading), each authored independently with its own epsilon, victim set, and enforcement profile; compare classifications across the family.',
    'Under the rangatiratanga reading the same arrangement computes as assumption without cession — larger victim set, higher epsilon, classification pulled toward pure extraction. Under the partnership reading it computes as constrained co-management — lower epsilon, victims partially converted to participants. This file''s classification holds only within this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings instantiate different constraints.').

omega_variable(
    textual_primacy_of_english_article_one,
    'Did the parties at Waitangi agree to what the English text records (''complete sovereignty'') or to what the Maori text records (kawanatanga, with te tino rangatiratanga guaranteed)?',
    'Historical-linguistic reconstruction of the February 1840 discussions: the missionaries'' translation choices, William Colenso''s record of rangatira objections, and the oral testimony the Waitangi Tribunal has gathered since 1985.',
    'If the Maori text reflects the understanding actually shared in 1840, the cession reading loses its textual foundation and the arrangement is better modeled as authority assumed rather than ceded — raising epsilon and shifting the classification toward pure extraction. If the English text reflects it, this file''s framing stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_primacy_of_english_article_one, empirical, 'Which of the two texts records what was actually agreed — the load-bearing factual question beneath this reading.').

omega_variable(
    persistence_basis_consent_inertia_enforcement,
    'Does the arrangement currently persist because of ongoing assent, constitutional inertia, or active enforcement?',
    'Track enforcement-side legislative and fiscal activity defending the reading (the Treaty Principles Bill episode, litigation posture on co-governance) against the volume of settled grievances, cross-party support for the status quo, and the absence of any serious repeal mechanism.',
    'Inertia-dominant persistence predicts atrophy toward theatrical maintenance of a function nobody defends; enforcement-dominant persistence predicts hardening and rising suppression. The two trajectories date different type transitions, so getting this wrong dates any drift verdict wrongly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_basis_consent_inertia_enforcement, empirical, 'What currently holds the arrangement in place — assent, inertia, or enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 1840, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wsa_crown_reading_tr_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1840, 0.2).
narrative_ontology:measurement(wsa_crown_reading_tr_t1870, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1870, 0.35).
narrative_ontology:measurement(wsa_crown_reading_tr_t1900, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1900, 0.45).
narrative_ontology:measurement(wsa_crown_reading_tr_t1930, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1930, 0.48).
narrative_ontology:measurement(wsa_crown_reading_tr_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1960, 0.5).
narrative_ontology:measurement(wsa_crown_reading_tr_t1975, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1975, 0.42).
narrative_ontology:measurement(wsa_crown_reading_tr_t1985, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(wsa_crown_reading_tr_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(wsa_crown_reading_tr_t2025, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2025, 0.33).

% Extraction over time
narrative_ontology:measurement(wsa_crown_reading_be_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1840, 0.42).
narrative_ontology:measurement(wsa_crown_reading_be_t1870, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1870, 0.7).
narrative_ontology:measurement(wsa_crown_reading_be_t1900, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1900, 0.76).
narrative_ontology:measurement(wsa_crown_reading_be_t1930, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1930, 0.73).
narrative_ontology:measurement(wsa_crown_reading_be_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1960, 0.68).
narrative_ontology:measurement(wsa_crown_reading_be_t1975, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1975, 0.64).
narrative_ontology:measurement(wsa_crown_reading_be_t1985, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement(wsa_crown_reading_be_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2000, 0.56).
narrative_ontology:measurement(wsa_crown_reading_be_t2025, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(wsa_crown_reading_su_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1840, 0.3).
narrative_ontology:measurement(wsa_crown_reading_su_t1870, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1870, 0.8).
narrative_ontology:measurement(wsa_crown_reading_su_t1900, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1900, 0.65).
narrative_ontology:measurement(wsa_crown_reading_su_t1930, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1930, 0.55).
narrative_ontology:measurement(wsa_crown_reading_su_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1960, 0.5).
narrative_ontology:measurement(wsa_crown_reading_su_t1975, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1975, 0.45).
narrative_ontology:measurement(wsa_crown_reading_su_t1985, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1985, 0.38).
narrative_ontology:measurement(wsa_crown_reading_su_t2000, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2000, 0.32).
narrative_ontology:measurement(wsa_crown_reading_su_t2025, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what Article I of the Treaty did' covers three structurally distinct claims and was decomposed per the epsilon-invariance principle into three linked stories sharing the waitangi_sovereignty_allocation kernel. This story carries the crown_sovereignty_reading's epsilon (0.58: contested, partially remediated, referent = plenary supremacy as practiced). The rangatiratanga_reading story carries a higher epsilon over a usurpation-shaped referent with a wider victim set; the partnership_reading story carries a lower epsilon over a constrained-co-management referent. The upstream reading (this one) has historically shaped the operating environment of the downstream readings — what consultation can mean, what the Tribunal can recommend — which is why the edges run from this story to both siblings. Each member links to the others; no orphan stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
