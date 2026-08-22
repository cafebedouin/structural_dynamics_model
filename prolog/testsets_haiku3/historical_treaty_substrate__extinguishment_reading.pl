% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__extinguishment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__extinguishment_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: historical_treaty_substrate__extinguishment_reading
 *   human_readable: Treaty Extinguishment Doctrine—Cession as Completed Property Transfer
 *   domain: legal/constitutional/indigenous_law
 *
 * SUMMARY:
 *   This constraint instantiates the extinguishment reading of the historical
 *   treaty substrate: Indigenous nations negotiated treaties with settler
 *   colonial states, understood as permanent cession of territorial
 *   sovereignty in exchange for defined reserve lands and annuities. Under
 *   this reading, the treaties constitute completed property
 *   transactions—Indigenous nations sold their territory and cannot reopen
 *   the transaction; the settler state possesses sole legitimate authority
 *   over ceded lands. This reading emerged in early colonial law to provide
 *   legal cover for dispossession and has persisted as institutional doctrine
 *   even as its founding justification (jurisdictional chaos requiring
 *   settlement) has become obsolete. The constraint exhibits the pathological
 *   markers of a zombie: high extraction (0.89), rising theater ratio (0.78,
 *   indicating performative maintenance outpacing real function), mounting
 *   suppression (0.92 at interval end), and a foundational contradiction—the
 *   doctrine claims to solve a historical problem
 *   (founding_problem_status=dead) that no longer requires solving, yet
 *   remains indispensable to the settler state's legitimacy. The coercion
 *   grid shows differential suppression across levels: individual-level
 *   suppression is highest (0.94), class-level resistance is mounting (0.81
 *   at 2026), and structural-level resistance has plateaued (0.58),
 *   indicating the constraint maintains itself through internalized
 *   suppression of individuals and isolation of Indigenous nations from
 *   organizational and class coalitions. This constraint is one reading of
 *   kernel historical_treaty_substrate; sibling readings
 *   (nation_to_nation_reading, stewardship_reading) would dramatically
 *   restructure the beneficiary/victim configuration and territorial
 *   authority.
 *
 * KEY AGENTS:
 *   - settler_state (institutional agenda_setter, powerless victims' counterparty): interprets treaties as completed cessions; maintains extinguishment doctrine through courts and policy; collects benefit of unilateral territorial control and resource extraction authority; exit from this reading would require renegotiating governance structures
 *   - indigenous_nations (powerless payer, nominally beneficiary): bear the cost of permanent cession interpretation; locked into identity (rejecting the doctrine is rejecting settlement's imposed legitimacy frame); nominally benefit from reserves and annuities positioned as compensation, not rights
 *   - settler_legal_establishment (institutional agenda_setter): judges, scholars, officials maintaining extinguishment doctrine through precedent and interpretive authority; professional identity depends on the doctrine's coherence
 *   - land_and_resource_claimants (powerful beneficiary): settlers, corporations, governments holding property titles and extraction licenses; benefit from legal certainty the extinguishment doctrine provides; would face restitution claims if the doctrine collapsed
 *   - international_human_rights_bodies (institutional observer, analytical): increasingly challenge extinguishment doctrine's compatibility with contemporary international law (UNDRIP, ILO 169); lack binding enforcement power but provide external criticism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, 0.89).
domain_priors:suppression_score(historical_treaty_substrate__extinguishment_reading, 0.92).
domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, snare).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Treaty Extinguishment Doctrine—Cession as Completed Property Transfer").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal/constitutional/indigenous_law").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, '11604bf3-5653-4e6b-b91e-f71c38aefe54').
narrative_ontology:cs_kernel_codification('11604bf3-5653-4e6b-b91e-f71c38aefe54', fixed_text).
narrative_ontology:cs_authority_grounding('11604bf3-5653-4e6b-b91e-f71c38aefe54', extraction).
narrative_ontology:cs_interpretation_layer_present('11604bf3-5653-4e6b-b91e-f71c38aefe54').
narrative_ontology:cs_reading_relation('11604bf3-5653-4e6b-b91e-f71c38aefe54', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_reading_relation('11604bf3-5653-4e6b-b91e-f71c38aefe54', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('11604bf3-5653-4e6b-b91e-f71c38aefe54', foundational, sovereignty_alienable_permanent_transaction).
narrative_ontology:cs_axiom_status(sovereignty_alienable_permanent_transaction, holdable).
narrative_ontology:cs_axiom_grounding('11604bf3-5653-4e6b-b91e-f71c38aefe54', sovereignty_alienable_permanent_transaction, conventional).
narrative_ontology:cs_axiom('11604bf3-5653-4e6b-b91e-f71c38aefe54', foundational, settler_state_sole_legitimate_territorial_authority).
narrative_ontology:cs_axiom_status(settler_state_sole_legitimate_territorial_authority, overridden).
narrative_ontology:cs_axiom_grounding('11604bf3-5653-4e6b-b91e-f71c38aefe54', settler_state_sole_legitimate_territorial_authority, conventional).
narrative_ontology:cs_reference_frame('11604bf3-5653-4e6b-b91e-f71c38aefe54', historical_treaties_as_complete_sovereignty_transfer).
narrative_ontology:cs_drift_state('11604bf3-5653-4e6b-b91e-f71c38aefe54', contemporary_indigenous_rights_era_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('11604bf3-5653-4e6b-b91e-f71c38aefe54', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_state).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_colonialist_doctrine).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_peoples).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, land_and_resource_claimants).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, absolute_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, terra_nullius_adjacent_framing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and administers treaties under the extinguishment doctrine: past Indigenous cessions are read as completed conveyances of territorial sovereignty, leaving the state as the sole legitimate authority over ceded lands. Defends this interpretation in courts, administrative proceedings, and policy. Collects the benefit of unencumbered territorial control, resource extraction rights, and the ability to unilaterally alter or ignore treaty obligations deemed 'historical.' The state's exit from this interpretation would require acknowledging ongoing Indigenous sovereignty and renegotiating governance structures.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_state, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Under the extinguishment reading, Indigenous nations are framed as having permanently ceded territorial sovereignty in exchange for reserve lands and annuities—a completed historical transaction that cannot be reopened. They bear the cost of this interpretation through loss of jurisdiction over traditional territories, exclusion from resource decisions, and legal inability to enforce treaty promises. They nominally benefit from reserve lands and annuity payments, but these are positioned as compensation for a concluded sale, not as ongoing rights. Exit from this reading is identity-locked: rejecting the extinguishment doctrine means rejecting the legal framework the settler state imposed to legitimize territorial dispossession.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_nations, payer,
    powerless, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, indigenous_nations, beneficiary).

% Bear the ongoing costs of the extinguishment interpretation: dispossession from ancestral territories, exclusion from resource benefits, subordination within state legal hierarchies, and cultural erosion tied to land loss. The extinguishment doctrine encodes their dispossession as a legal fact that cannot be contested, making exit conceptually and institutionally impossible—rejecting it would require rejecting the entire legitimacy framework imposed by settlement.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_peoples, payer,
    powerless, civilizational, identity_locked, national).

% The extinguishment reading vindicates the doctrine that colonial settlement, once consolidated, can convert Indigenous territory into settler property through a single historical transaction. This doctrine is not an agent but a set of propositions: that treaties can extinguish sovereignty, that cession is irreversible, that settler occupation creates legitimate property claims superseding prior Indigenous use. The doctrine persists because the institutional framework built on it would be delegitimized by its rejection.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_colonialist_doctrine, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(historical_treaty_substrate__extinguishment_reading, settler_colonialist_doctrine).

% Judges, legal scholars, and administrative officials who interpret and apply the extinguishment doctrine in courts and policy. They maintain the doctrine through precedent, statutory construction, and interpretive authority. Their professional identity and institutional position depend on the doctrine's coherence; rejecting it would require delegitimizing decades or centuries of judicial rulings and legal scholarship.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_legal_establishment, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Settler individuals, corporations, and governments who hold property titles, resource extraction licenses, and development rights on ceded territories. They benefit from the extinguishment doctrine because it provides legal certainty that their claims are not subject to challenge from Indigenous nations. Exit from the extinguishment reading would expose them to claims for restitution or shared jurisdiction.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, land_and_resource_claimants, beneficiary,
    powerful, generational, mobile, national).

% Indigenous nations, scholars, and legal advocates who argue for nation-to-nation or stewardship readings of treaties. They are structurally excluded from the interpretation authority that the extinguishment doctrine grants to the settler state, though their voices increasingly appear in litigation and policy discourse. They would fundamentally restructure the constraint if admitted to interpretive authority.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_sovereignty_advocates, excluded,
    moderate, civilizational, constrained, national).

% UN bodies, international courts, and treaty bodies that monitor Indigenous rights and have increasingly challenged the extinguishment doctrine's compatibility with contemporary international law (UNDRIP, ILO Convention 169, etc.). They observe and critique the settler state's adherence to extinguishment but lack direct enforcement power; their recommendations have no binding force on domestic law.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__extinguishment_reading, settler_state).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__extinguishment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The extinguishment doctrine presents itself as solving a coordination problem (defining territorial boundaries after settlement), but this function was real only in the early colonial period when jurisdictional boundaries were genuinely contested. By the 20th century, territorial control was consolidated and the doctrine's function shifted entirely to defending the settler state's unilateral interpretation against Indigenous legal claims and international scrutiny. The doctrine is now purely extractive theater.
% TRANSFER_FUNCTION: Moves territorial sovereignty, resource extraction rights, jurisdictional authority, and political control from Indigenous nations to the settler state. In exchange, Indigenous nations receive defined reserve lands (typically 1-10% of original territory) and annuities (often unpaid, inadequate, or unilaterally altered). The transfer is presented as a completed historical transaction that cannot be reopened, making it irreversible even if the original agreement was obtained under duress or misunderstood by Indigenous signatories.
% ABSENT_VOICES: Indigenous nations' own lawyers, scholars, and political leaders who argue for nation-to-nation or stewardship readings are structurally excluded from the settler courts' interpretive authority—they appear as witnesses or parties to litigation but not as co-interpreters of the law itself. International human rights bodies offer alternative readings but lack binding enforcement power. Settler legal scholars who critique the extinguishment doctrine are present in academic discourse but lack institutional authority in courts and policy. The extinguishment reading's dominance is maintained through institutional authority, not through persuasiveness on the merits.
% DISAPPEARANCE_RATIONALE: If the extinguishment doctrine disappeared—if treaties were recognized as ongoing nation-to-nation agreements or relational stewardship pacts—the territorial and jurisdictional foundations of settler-colonial states would be destabilized. Indigenous nations would regain jurisdictional claims to vast territories; land titles would be unsettled; resource extraction would require ongoing Indigenous consent; the settler state's unilateral authority would be structurally limited. The disappearance of this single constraint would reorganize the entire territorial, political, and economic order of settler-colonial societies.
% FOUNDING_PROBLEM: Early colonial settlement (roughly 1763-1850) created jurisdictional chaos: settlers occupied Indigenous territories, established occupation-based claims, and challenged Indigenous authority. Treaties were negotiated to create the appearance of legitimate transfer and to induce Indigenous nations to accept defined reserve boundaries. The extinguishment doctrine emerged as a legal mechanism to convert these treaties into irreversible property transactions, protecting settler claims from reopening and establishing the settler state as the sole legitimate territorial authority.
% FOUNDING_PROBLEM_CORROBORATION: The settler state and its legal establishment attest the founding problem remains live (need for territorial certainty, legal stability, protection of property rights). However, historical evidence, Indigenous sources, international human rights bodies (UNDRIP, Permanent Forum on Indigenous Issues), and settler scholars of colonialism attest the founding problem was essentially solved by 1920—territorial control was consolidated, jurisdictional boundaries were established, and the settler state's dominance was institutionalized. The extinguishment doctrine now persists not because territorial chaos requires it but because the settler state benefits from it. This mismatch (founding_problem_status=dead + disappearance_verdict=world_rearranges) is the diagnostic marker of the constraint as a zombie: it persists even though the problem it was built to solve no longer exists.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__extinguishment_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__extinguishment_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__extinguishment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(historical_treaty_substrate__extinguishment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__extinguishment_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__extinguishment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__extinguishment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.68→0.89 over 263 years) because the constraint's primary function has never been coordination—it is pure extraction of territorial sovereignty and resource rights. The settler state unilaterally defined the terms, obtained Indigenous assent under duress/information asymmetry, and then declared the transaction irreversible. As alternatives emerged (nation-to-nation readings, international human rights law, other settler-colonial states' moves toward cession-reversal), the settler state invested MORE enforcement effort to maintain the doctrine, not less. Suppression is extremely high (0.92) because the constraint's persistence depends on actively preventing Indigenous legal claims, silencing alternative interpretations, and blocking international human rights scrutiny. Theater ratio is high and rising (0.20→0.78), indicating the constraint increasingly operates through performative maintenance (courts repeatedly reaffirming the doctrine in response to mounting challenges) rather than through actual coordination or even straightforward coercion. By 2026, the constraint is primarily theater: it cannot be defended on its original merits (the jurisdictional chaos of early colonization is gone), requires constant judicial and legislative reinforcement against rising challenges, and persists largely because the settler state's entire territorial and institutional framework rests on it. This is the signature pathology of the piton class—an atrophied coordination mechanism maintained through institutional inertia, theater, and suppression. However, the claimed_type is snare, not piton, because: (1) there IS a concentrated beneficiary (the settler state) capturing the extraction, not diffuse benefit to administrators; (2) the constraint persists not because no one wants to fix it but because the beneficiary has the power to prevent fixing it; (3) victims are identifiable and structured, not diffuse. The distinction matters: a piton has no concentrated beneficiary; a snare extracts to a named actor with power.
 *
 * PERSPECTIVAL GAP:
 *   From the settler_state and settler_legal_establishment seats, the extinguishment doctrine appears as coherent legal history and current law—a settled matter on which institutional stability depends. From the indigenous_nations seat, it appears as a constructed fiction imposed to legitimize expropriation. From the international_human_rights_bodies seat, it violates contemporary international law. Each seat has a different EXIT PROFILE: settler state exit=arbitrage (could switch to nation-to-nation reading, would only lose some leverage); Indigenous nations exit=identity_locked (rejecting the doctrine means rejecting the only legal framework recognizing any Indigenous position); international bodies exit=analytical (no domestic enforcement power). The engine derives directionality from exit + power + role, so these divergences will produce different per-seat types.
 *
 * DIRECTIONALITY LOGIC:
 *   The settler state is the structural beneficiary: it collects territorial control, resource rights, jurisdiction, and the ability to unilaterally interpret and enforce the doctrine. Its directionality is low (near 0.0 = full beneficiary). Indigenous nations are the structural targets: they bear the costs (dispossession, jurisdictional exclusion, inability to enforce treaty terms against settler state reinterpretation) in exchange for narrow benefits (reserve lands, annuities) that are positioned as compensation for a concluded sale, not as ongoing rights. Their directionality is high (near 1.0 = full target). The internalized suppression is what makes this particularly extractive: Indigenous nations have come to partially accept the extinguishment narrative (reserve lands as 'ours,' annuities as 'owed,' treaty as 'settled'), which makes exit structurally and psychologically impossible—rejecting the doctrine would require rejecting the legal framework that has become the only recognized avenue for asserting any territorial claims. This is why exit_options=identity_locked for Indigenous nations: the constraint does not appear as external coercion but as the internalized terms of Indigenous existence within the settler state.
 *
 * MANDATROPHY ANALYSIS:
 *   The extinguishment reading exhibits classic mandatrophy: founding_problem_status=dead (the jurisdictional chaos of early colonization is long solved) but disappearance_verdict=world_rearranges (the constraint's removal would structurally reorganize settler territory and authority). This mismatch—the constraint persists even though the problem it was built to solve no longer exists—is the diagnostic of a zombie or degraded constraint. The settler state defends the doctrine by constantly reasserting it in courts, not by pointing to current coordination problems (which do not exist); it maintains the doctrine through theater (repeated litigation, judicial reaffirmations) not through efficiency. The high theater_ratio (0.78) confirms this: the constraint is increasingly performative maintenance. However, the constraint does NOT compute as piton because there IS a concentrated beneficiary (settler state) who has the power and incentive to maintain it—pitons lack concentrated beneficiaries, surviving through diffuse administrative inertia. The extinguishment doctrine persists not because fixing it would be expensive relative to benefit, but because the beneficiary has the structural power to prevent fixing it. This is snare geometry: concentrated extraction defended through suppression, not piton geometry (diffuse costs, no concentrated beneficiary, cheap to fix but no one with sufficient power to advocate for fixing).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extinguishment_vs_constructed_reading,
    'Is the extinguishment reading a factual account of what the original treaties meant and intended (a discovered fact), or is it a legal interpretation imposed by settler courts after the fact to consolidate settler control (a constructed framing)?',
    'Comparative analysis of (a) the written treaty texts themselves; (b) contemporary Indigenous understandings recorded in oral tradition, letters, and historical accounts; (c) settler negotiators'' private correspondence about their intent; (d) parallel treaties in other settler-colonial jurisdictions and how those have been reinterpreted. If Indigenous sources consistently describe relational or limited-use agreements while settler sources consistently describe complete cession, the reading is constructed, not discovered.',
    'If the extinguishment reading is constructed rather than factually grounded, it loses its claim to settled historical fact and becomes contestable as a policy choice—a deliberate legal interpretation chosen to legitimize dispossession, not a discovered truth. This would support nation-to-nation and stewardship readings as equally valid or more valid interpretations of the same historical agreements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extinguishment_vs_constructed_reading, empirical, 'Whether the extinguishment reading is historical fact or imposed legal interpretation.').

omega_variable(
    duress_and_consent_validity,
    'Were the original treaties executed with valid Indigenous consent, or under duress (military threat, starvation, broken prior agreements, information asymmetry)?',
    'Historical documentation of conditions under which treaties were signed: military presence, food security, prior violated agreements, capacity for informed negotiation. Contemporary contract law recognizes duress as vitiating consent; international law recognizes treaties obtained by coercion as void.',
    'If the original treaties were obtained under duress, they would be voidable under both common law and international law, making the extinguishment reading legally indefensible even on its own terms (you cannot buy property from someone under gunpoint and claim permanent ownership). This would support reopening all treaty claims and restructuring the constraint entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(duress_and_consent_validity, empirical, 'Whether original treaty consent was valid or vitiated by duress.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.92) structural (legal barriers, institutional exclusion, geographic isolation from alternatives) or internalized (Indigenous peoples have come to accept the extinguishment narrative as legitimate, making exit psychologically/culturally impossible even if structural barriers were removed)?',
    'Post-barrier-removal observation: jurisdictions that have formally recognized Indigenous sovereignty while maintaining settler institutions (e.g., New Zealand post-Treaty of Waitangi reinterpretation, some Canadian provincial moves toward co-jurisdiction) show whether Indigenous nations and peoples exit the constraint or remain embedded in it. If they remain embedded despite barrier removal, suppression is substantially internalized.',
    'If suppression is internalized, the constraint''s effective suppression is higher than the structural measure suggests—the victims carry the suppression with them even if the settler state removed legal barriers. This would require addressing cultural and institutional decolonization, not just legal reform, to reduce suppression. If suppression is purely structural, legal barrier removal would immediately reduce the constraint''s grip.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether the high suppression is structural or internalized.').

omega_variable(
    mandatrophy_zombie_identification,
    'Is the extinguishment doctrine a zombie constraint—maintained by institutional inertia even though the problem it was built to solve (early colonial jurisdictional chaos) is long extinct—or does it serve an ongoing extractive function that justifies its current enforcement?',
    'Examination of the doctrine''s stated justifications in contemporary judicial opinions and policy: Do settler courts cite jurisdictional necessity or territorial stability as current justifications? Or do they rely on historical precedent and ''settled law'' without articulating a current coordination problem? If the latter, the doctrine is zombie.',
    'If the doctrine is zombie, it is maintained primarily through theater and beneficiary power (settler state refuses to abandon a profitable arrangement), not through genuine necessity. This supports piton classification if no concentrated beneficiary exists, or snare classification if the settler state is the identifiable extractor. The constraint''s continued enforcement would become purely predatory, without even the fig leaf of solving a real problem.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_zombie_identification, empirical, 'Whether the founding problem is truly dead or the doctrine serves ongoing extractive function.').

omega_variable(
    kernel_reading_contestation,
    'Are the nation_to_nation_reading and stewardship_reading equally valid interpretations of the same historical kernel, or does the extinguishment reading''s control of interpretive authority make it the legally dominant reading regardless of comparative validity?',
    'Comparison of: (a) the logical coherence and textual support for each reading within the treaty texts themselves; (b) which reading is recognized in international law (UNDRIP, ILO 169); (c) which reading is recognized in comparable settler-colonial jurisdictions; (d) which reading produces outcomes consistent with contemporary human rights standards. If non-extinguishment readings are more coherent, more internationally recognized, and more rights-respecting, the extinguishment reading''s dominance is an artifact of settler-state power, not interpretive validity.',
    'If the extinguishment reading is dominant only because the settler state controls interpretive authority, not because it is more valid on the merits, the reading''s legitimacy is vulnerable to challenge—particularly as international law shifts and as comparative precedent from other settler-colonial states accumulates. This supports the sibling readings'' eventual ascendancy and structural reorganization of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether extinguishment dominates through interpretive authority (power) or interpretive validity (coherence).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 1763, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1763, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1763, 0.2).
narrative_ontology:measurement_basis(hist_tr_t1763, projected).
narrative_ontology:measurement(hist_tr_t1850, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1850, 0.35).
narrative_ontology:measurement_basis(hist_tr_t1850, observed).
narrative_ontology:measurement(hist_tr_t1920, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1920, 0.52).
narrative_ontology:measurement_basis(hist_tr_t1920, observed).
narrative_ontology:measurement(hist_tr_t1970, historical_treaty_substrate__extinguishment_reading, theater_ratio, 1970, 0.68).
narrative_ontology:measurement_basis(hist_tr_t1970, observed).
narrative_ontology:measurement(hist_tr_t2000, historical_treaty_substrate__extinguishment_reading, theater_ratio, 2000, 0.74).
narrative_ontology:measurement_basis(hist_tr_t2000, observed).
narrative_ontology:measurement(hist_tr_t2026, historical_treaty_substrate__extinguishment_reading, theater_ratio, 2026, 0.78).
narrative_ontology:measurement_basis(hist_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(hist_be_t1763, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1763, 0.68).
narrative_ontology:measurement_basis(hist_be_t1763, projected).
narrative_ontology:measurement(hist_be_t1850, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1850, 0.76).
narrative_ontology:measurement_basis(hist_be_t1850, observed).
narrative_ontology:measurement(hist_be_t1920, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1920, 0.82).
narrative_ontology:measurement_basis(hist_be_t1920, observed).
narrative_ontology:measurement(hist_be_t1970, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 1970, 0.85).
narrative_ontology:measurement_basis(hist_be_t1970, observed).
narrative_ontology:measurement(hist_be_t2000, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 2000, 0.87).
narrative_ontology:measurement_basis(hist_be_t2000, observed).
narrative_ontology:measurement(hist_be_t2026, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 2026, 0.89).
narrative_ontology:measurement_basis(hist_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1763, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1763, 0.72).
narrative_ontology:measurement_basis(hist_su_t1763, observed).
narrative_ontology:measurement(hist_su_t1850, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1850, 0.81).
narrative_ontology:measurement_basis(hist_su_t1850, observed).
narrative_ontology:measurement(hist_su_t1920, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1920, 0.86).
narrative_ontology:measurement_basis(hist_su_t1920, observed).
narrative_ontology:measurement(hist_su_t1970, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 1970, 0.88).
narrative_ontology:measurement_basis(hist_su_t1970, observed).
narrative_ontology:measurement(hist_su_t2000, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement_basis(hist_su_t2000, observed).
narrative_ontology:measurement(hist_su_t2026, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 2026, 0.92).
narrative_ontology:measurement_basis(hist_su_t2026, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1763, tn=2026
narrative_ontology:measurement(hist_grid_01, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(class), 1763, 0.58).
narrative_ontology:measurement(hist_grid_02, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(class), 2026, 0.85).
narrative_ontology:measurement(hist_grid_03, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(individual), 1763, 0.68).
narrative_ontology:measurement(hist_grid_04, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(individual), 2026, 0.89).
narrative_ontology:measurement(hist_grid_05, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(organizational), 1763, 0.64).
narrative_ontology:measurement(hist_grid_06, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(organizational), 2026, 0.87).
narrative_ontology:measurement(hist_grid_07, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(structural), 1763, 0.71).
narrative_ontology:measurement(hist_grid_08, historical_treaty_substrate__extinguishment_reading, accessibility_collapse(structural), 2026, 0.91).
narrative_ontology:measurement(hist_grid_09, historical_treaty_substrate__extinguishment_reading, resistance(class), 1763, 0.72).
narrative_ontology:measurement(hist_grid_10, historical_treaty_substrate__extinguishment_reading, resistance(class), 2026, 0.81).
narrative_ontology:measurement(hist_grid_11, historical_treaty_substrate__extinguishment_reading, resistance(individual), 1763, 0.58).
narrative_ontology:measurement(hist_grid_12, historical_treaty_substrate__extinguishment_reading, resistance(individual), 2026, 0.62).
narrative_ontology:measurement(hist_grid_13, historical_treaty_substrate__extinguishment_reading, resistance(organizational), 1763, 0.48).
narrative_ontology:measurement(hist_grid_14, historical_treaty_substrate__extinguishment_reading, resistance(organizational), 2026, 0.76).
narrative_ontology:measurement(hist_grid_15, historical_treaty_substrate__extinguishment_reading, resistance(structural), 1763, 0.61).
narrative_ontology:measurement(hist_grid_16, historical_treaty_substrate__extinguishment_reading, resistance(structural), 2026, 0.58).
narrative_ontology:measurement(hist_grid_17, historical_treaty_substrate__extinguishment_reading, stakes_inflation(class), 1763, 0.72).
narrative_ontology:measurement(hist_grid_18, historical_treaty_substrate__extinguishment_reading, stakes_inflation(class), 2026, 0.88).
narrative_ontology:measurement(hist_grid_19, historical_treaty_substrate__extinguishment_reading, stakes_inflation(individual), 1763, 0.68).
narrative_ontology:measurement(hist_grid_20, historical_treaty_substrate__extinguishment_reading, stakes_inflation(individual), 2026, 0.86).
narrative_ontology:measurement(hist_grid_21, historical_treaty_substrate__extinguishment_reading, stakes_inflation(organizational), 1763, 0.62).
narrative_ontology:measurement(hist_grid_22, historical_treaty_substrate__extinguishment_reading, stakes_inflation(organizational), 2026, 0.84).
narrative_ontology:measurement(hist_grid_23, historical_treaty_substrate__extinguishment_reading, stakes_inflation(structural), 1763, 0.55).
narrative_ontology:measurement(hist_grid_24, historical_treaty_substrate__extinguishment_reading, stakes_inflation(structural), 2026, 0.78).
narrative_ontology:measurement(hist_grid_25, historical_treaty_substrate__extinguishment_reading, suppression(class), 1763, 0.68).
narrative_ontology:measurement(hist_grid_26, historical_treaty_substrate__extinguishment_reading, suppression(class), 2026, 0.92).
narrative_ontology:measurement(hist_grid_27, historical_treaty_substrate__extinguishment_reading, suppression(individual), 1763, 0.71).
narrative_ontology:measurement(hist_grid_28, historical_treaty_substrate__extinguishment_reading, suppression(individual), 2026, 0.94).
narrative_ontology:measurement(hist_grid_29, historical_treaty_substrate__extinguishment_reading, suppression(organizational), 1763, 0.71).
narrative_ontology:measurement(hist_grid_30, historical_treaty_substrate__extinguishment_reading, suppression(organizational), 2026, 0.93).
narrative_ontology:measurement(hist_grid_31, historical_treaty_substrate__extinguishment_reading, suppression(structural), 1763, 0.76).
narrative_ontology:measurement(hist_grid_32, historical_treaty_substrate__extinguishment_reading, suppression(structural), 2026, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__extinguishment_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(historical_treaty_substrate__extinguishment_reading, 0.25).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate__nation_to_nation_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate__stewardship_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, indian_reserve_land_subordination).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, terra_nullius_doctrine__settler_legal).

% DUAL FORMULATION NOTE:
% The historical_treaty_substrate kernel is instantiated in three separate constraint stories, each representing a competing reading of the same historical agreements: extinguishment_reading (this story), nation_to_nation_reading, and stewardship_reading. These three readings coexist as live positions held by different institutional and Indigenous factions across settler-colonial jurisdictions. The extinguishment_reading claims Indigenous nations permanently ceded sovereignty; nation_to_nation_reading claims treaties bind ongoing nation-to-nation consultation; stewardship_reading claims treaties establish relational coexistence without cession. Each story has distinct ε values reflecting the different structural extractions and distributions visible from each reading's frame. Decomposition is required because the ε values differ by a wide margin: extinguishment (ε ≈ 0.89) frames high extraction as historical property transfer; nation-to-nation (ε ≈ 0.60-0.70) frames the same arrangements as violated ongoing agreements; stewardship (ε ≈ 0.50-0.60) frames them as relational pacts with shared obligation. They share the same referent (the historical treaties and their current interpretation) but assess that referent through incompatible frameworks, yielding incompatible ε values. All three stories link via network.affects_constraints to enable constraint-family analysis and contamination propagation tracking.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historical_treaty_substrate__extinguishment_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
