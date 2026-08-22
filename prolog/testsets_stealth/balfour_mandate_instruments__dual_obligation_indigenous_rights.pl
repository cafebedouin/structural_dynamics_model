% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__dual_obligation_indigenous_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Mandate Dual-Obligation Regime: Indigenous Rights and Land-Tenure Protection
 *   domain: international law / colonial administration / state formation
 *
 * SUMMARY:
 *   A League of Nations mandatory administration governs a territory under
 *   instruments that bind it to protect the existing Arab population's civil
 *   and political rights and land tenure on terms equal to — and where they
 *   conflict, superior to — the undertaking to foster a Jewish national home.
 *   The operative machinery is concrete: registered tenure and a cultivators'
 *   protection ordinance shield tenancy from eviction upon sale; zoning
 *   regulations close whole regions of the countryside to acquisition by the
 *   buying institutions; an annual certificate schedule paces Jewish
 *   immigration to the economy's absorptive capacity; and a petition channel
 *   runs from the Arab population to the League's supervisory commission in
 *   Geneva. The arrangement coordinates the contact of two peoples living
 *   under incompatible wartime undertakings, and it does so at asymmetric
 *   cost: the seats that pay are the institutions whose acquisition and
 *   settlement programs the restrictions bind, the refugees whom the quota
 *   turns back, and the administration itself, which funds and staffs the
 *   machinery while collecting no revenue from it. The claim/metric gap is
 *   deliberate: the arrangement is CLAIMED as tangled_rope — genuine
 *   coordination carrying asymmetric extraction through the same instruments
 *   — and the metrics are authored independently from the enforcement record;
 *   the engine computes each seat's classification from the structural data.
 *   KEY AGENTS (by structural relationship): -
 *   british_mandatory_administration: agenda-setter and constrained payer
 *   (institutional/constrained) — drafts and enforces the protective
 *   instruments while absorbing their fiscal and diplomatic costs -
 *   palestinian_arab_tenant_farmers: primary beneficiary (powerless/trapped)
 *   — occupancy protected by tenancy ordinance and transfer zoning -
 *   palestinian_arab_landowning_families: dual-positioned beneficiary
 *   (organized/constrained) — protected title, capped sale prices -
 *   palestinian_arab_national_elites: beneficiary of preserved majority
 *   standing (organized/identity_locked) - zionist_institutions: primary
 *   target (powerful/constrained) — land acquisition zoned out, immigration
 *   capped - prospective_jewish_immigrants: target (powerless/trapped) — the
 *   certificate schedule binds their escape routes -
 *   league_of_nations_permanent_mandates_commission: analytical observer
 *   (institutional/analytical) — audits compliance, commands no enforcement -
 *   pan_arab_congress_movements: excluded (organized/mobile) — rejects the
 *   framework entire, no seat in its consultations
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.6).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.38).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.56).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.6).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.56).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Mandate Dual-Obligation Regime: Indigenous Rights and Land-Tenure Protection").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international law / colonial administration / state formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'feee53b4-4953-43df-af04-773e43315c0c').
narrative_ontology:cs_kernel_codification('feee53b4-4953-43df-af04-773e43315c0c', fixed_text).
narrative_ontology:cs_authority_grounding('feee53b4-4953-43df-af04-773e43315c0c', lineage).
narrative_ontology:cs_interpretation_layer_present('feee53b4-4953-43df-af04-773e43315c0c').
narrative_ontology:cs_reading_relation('feee53b4-4953-43df-af04-773e43315c0c', balfour_mandate_instruments__jewish_national_home_primacy, forecloses).
narrative_ontology:cs_reading_relation('feee53b4-4953-43df-af04-773e43315c0c', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('feee53b4-4953-43df-af04-773e43315c0c', foundational, existing_population_rights_equal_or_superior).
narrative_ontology:cs_axiom_status(existing_population_rights_equal_or_superior, holdable).
narrative_ontology:cs_axiom_grounding('feee53b4-4953-43df-af04-773e43315c0c', existing_population_rights_equal_or_superior, deontological).
narrative_ontology:cs_axiom('feee53b4-4953-43df-af04-773e43315c0c', foundational, national_home_subordinate_to_self_determination).
narrative_ontology:cs_axiom_status(national_home_subordinate_to_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('feee53b4-4953-43df-af04-773e43315c0c', national_home_subordinate_to_self_determination, conventional).
narrative_ontology:cs_reference_frame('feee53b4-4953-43df-af04-773e43315c0c', dual_obligation_sacred_trust).
narrative_ontology:cs_drift_state('feee53b4-4953-43df-af04-773e43315c0c', mandate_final_decade, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('feee53b4-4953-43df-af04-773e43315c0c', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_tenant_farmers).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_landowning_families).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_national_elites).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_institutions).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, prospective_jewish_immigrants).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administration).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_landowning_families).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, league_covenant_sacred_trust_doctrine).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, minority_protection_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the land registries, immigration schedules, and tenancy courts through which the protective obligations operate, and drafts the white papers and regulations that set their terms. It collects no revenue from the arrangement — it pays to run it: garrison and coastal patrol costs, compensation schemes for displaced cultivators, and the diplomatic expense of disappointing both communities and both wings of its own domestic politics. Its exit is bounded by League supervision and the treaty character of the Mandate; relinquishing the trusteeship means surrendering the strategic position it holds the territory for.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administration, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administration, payer).

% Work and occupy land under registered tenancy; the cultivators' protection ordinance and the transfer zoning are what stand between them and eviction by sellers and buyers alike. They have no alternative livelihood off the land and no standing in the political process except through petitions drafted by others; when the protections lapse or are evaded, they absorb the loss directly.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_tenant_farmers, beneficiary,
    powerless, biographical, trapped, local).

% Hold registered title that the transfer regulations protect from alienation, and their tenancy-holding clients are shielded from eviction on their land. At the same time the zoning bars them from selling tracts in the closed zones to the highest bidder, capping the price their principal asset can command; several houses hedged by selling in the open zones or through intermediaries before the zones closed.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_landowning_families, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_landowning_families, payer).

% Lead the municipal councils, the Supreme Muslim Council, and the Arab Higher Committee; the preserved Arab majority is the demographic ground of their claim to representative government and eventual statehood, and the protective obligations are the legal form that claim currently takes. Their political standing is constituted by the communal claim — conceding the arrangements or leaving the country dissolves the position they hold. Their strategy space is petitioning, boycott, and revolt, not departure.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_national_elites, beneficiary,
    organized, generational, identity_locked, regional).

% The Jewish Agency, the Jewish National Fund, and the Zionist Executive raise diaspora funds to buy land and organize immigration; the transfer regulations close zones to them and the certificate schedule caps the inflow their settlement plans assume. They respond with front companies, purchases in the open zones, and political pressure in London rather than abandonment — the national-home project is their constitutive purpose, so stepping outside the arrangements means dissolving the enterprise. Their costs are paid in blocked acquisitions, inflated prices in the open zones, and years of spent diplomatic capital.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_institutions, payer,
    powerful, generational, constrained, global).

% European Jews, increasingly refugees, for whom an admission certificate is the difference between escape and staying; the quota schedule fixes the number of certificates, and the undocumented routes that substitute for them carry interception, detention, and deportation. They choose among waiting lists, smugglers, and other destinations that are closing as fast.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, prospective_jewish_immigrants, payer,
    powerless, immediate, trapped, global).

% Reviews the administration's annual reports, hears Arab petitions, and questions the accredited representative in Geneva; it can find the administration in breach of its obligations and refer matters to the Council, but commands no enforcement of its own. Its scrutiny is the external check on how the protective obligations are read and applied.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, league_of_nations_permanent_mandates_commission, observer,
    institutional, generational, analytical, global).

% Congresses in Damascus, Jerusalem, and Cairo demand full independence and repudiation of the Mandate framework altogether rather than its protective provisions; they are outside the instruments' consultation structure and address it only through protest and delegation. Their exclusion marks the boundary of the arrangement: it manages the two populations inside the territory and has no seat for the regional politics surrounding them.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, pan_arab_congress_movements, excluded,
    organized, generational, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__dual_obligation_indigenous_rights, diffuse).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__dual_obligation_indigenous_rights, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Governs a single territory containing two peoples living under incompatible wartime undertakings: land registration and tenancy protection prevent the rural collapse and mass dispossession that unregulated land markets were producing; the certificate schedule paces demographic change to what the existing economy and polity can absorb; petition channels to Geneva and statutory protections give the Arab population a legal alternative to revolt; and the whole apparatus holds the territory in trust pending a settlement.
% TRANSFER_FUNCTION: Moves land-acquisition opportunity and demographic capacity away from the Zionist institutions toward preservation of existing Arab tenure and majority standing; moves enforcement and compensation costs onto the British treasury; moves order and deferred dispossession to the Arab population; and moves nothing to the administrators, who pay to operate the machinery they run.
% ABSENT_VOICES: Pan-Arab congress movements, which reject the Mandate framework entire and would trade its protections for immediate independence, have no seat in the instruments' consultation structure. Inside the territory, the tenant farmers' voice reaches Geneva only through petitions drafted by urban notables and elites, so the recorded record over-represents the literate and propertied strata of the beneficiary population.
% DISAPPEARANCE_RATIONALE: Without the transfer zoning and the certificate schedule, land sales to the buying institutions accelerate immediately — the pre-restriction pace had already displaced whole villages in the valleys — the demographic balance shifts within a decade, the Arab majority claim loses its factual ground, and the territory's politics reorganize around accomplished displacement rather than managed coexistence; the administration simultaneously loses the legal frame that justifies its presence to Geneva.
% FOUNDING_PROBLEM: Reconcile, inside one governing instrument, the wartime undertaking to establish a Jewish national home with the League Covenant's sacred-trust duty toward the population already inhabiting the territory — preventing, in a League-supervised territory, a repeat of the settler dispossession the preceding century had made routine.
% FOUNDING_PROBLEM_CORROBORATION: British and international investigative bodies outside the beneficiary set attest both the obligations and their bindingness: the Shaw Commission (1929) and the Hope Simpson Report (1930) found the protective duties real and unfulfilled, and the Permanent Mandates Commission pressed the administration on them repeatedly across the interval. Zionist submissions disputed the obligations' priority but never their existence in the text. No source outside the benefiting parties attests that the founding problem was resolved.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__dual_obligation_indigenous_rights, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__dual_obligation_indigenous_rights, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high because the two operative mechanisms — the transfer zoning and the certificate schedule — deny the paying seats their core project inputs at identity-relevant scale, peaking in 1939-40 when the White Paper schedule and the Land Transfers Regulations closed both channels almost completely; the terminal decline reflects enforcement collapse under insurgency, not repeal. Suppression tracks the machinery needed to hold the arrangement: modest through the quiet 1920s, expanding with the Hope Simpson inspection regime, spiking with the emergency powers of the revolt years and the coastal interdiction of the 1940s, then falling as the administration loses the capacity to police anything — the suppression_requirement series is authored because this story specifically traces enforcement-capacity build-up and decay. Theater moves inversely to enforcement sincerity: lowest when declarations and application align (the 1922 clarification, the 1939-40 peak), spiking at the 1931 MacDonald letter, which kept the protective language while reversing its substance, and highest at the end, when officials cite rules they can no longer apply. All three series share one eight-point grid (four-year steps, 1920-1948) so every metric is authored at every examined time point. The dynamics are cyclical rather than monotonic: incident (riots 1929, revolt 1936) -> inquiry (Shaw, Peel) -> reform (Passfield, White Paper) -> relaxation or reversal (MacDonald 1931, wartime drift) — and the cycle itself functions as a burden-management mechanism, each tightening bought by crisis and each relaxation purchased by the paying seats' pressure. Accessibility collapse stays moderate because circumvention channels (front companies, open-zone purchases, undocumented immigration) never closed; resistance stays high because the organized paying seat mounted sustained opposition and the beneficiary population itself revolted when it judged the protections inadequately enforced. Suppression here is structural — registries, patrols, detention — not internalized; no suppression-mechanism omega is needed beyond the enforcement-sincerity omega already carried.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently because the same instruments are, from each position, a different fact. From the tenant-farmer seat the apparatus is the only thing between occupancy and eviction, and its lapses are experienced as betrayal by the trustee. From the Zionist institutional seat it is enforced denial of a founding undertaking, administered by a power that signed both documents. From the administration's own seat it is an unpayable double commitment: it wrote the restrictions, enforces them, and absorbs the fiscal and diplomatic cost of every term, collecting no revenue from any of it. The Arab national elites experience the arrangements as the legal form of a sovereignty claim they regard as already theirs; the Geneva commission experiences them as reports to be audited. Identity lock binds the elites and the Zionist institutions on opposite sides: each seat's political existence is constituted by the claim the arrangements adjudicate, so neither can exit without dissolving the actor — break that identity frame and both seats' exit options reopen, changing their computed positions materially.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation. Tenant farmers (trapped, powerless) sit nearest the full-beneficiary end — the protections subsidize them and they hold no alternative position. National elites (identity_locked) sit close behind, their benefit being the preserved demographic ground of their claim. Landowning families are genuinely dual-positioned — protected title on one side, capped sale prices on the other — placing them nearer the middle than the other beneficiary seats. The paying seats sit at the target end: prospective immigrants highest (trapped; the schedule binds them personally and, in the 1940s, fatally), Zionist institutions slightly below (powerful enough to buy partial relief through circumvention and lobbying, but constrained by their constitutive commitment). The administration is the anomalous seat: an agenda-setter whose role would ordinarily place it near the beneficiary end but whose actual position is that of a constrained payer — it funds the machinery, compensates the displaced, and spends diplomatic capital on every term, collecting nothing. The structural data carries this through its secondary payer role; no directionality override is authored because the role pair, not a power-atom correction, is what distinguishes it, and an override keyed to the institutional power atom would also misfire on the Geneva observer.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the apparatus as pure extraction would erase the documented protection — eviction restrictions that held in the registers, quota years that delivered the certificates' worth of refuge, petition channels that produced real findings — and mislabel a functioning, if asymmetric, governance structure as a trap. Reading it as pure coordination would erase the paying seats' burden and the administration's unrecovered costs. The tangled_rope classification holds both facts: a genuine coordination function (managing two populations' contact under incompatible undertakings) operating through the same instruments that impose asymmetric, identity-relevant costs on identifiable payers. On mandatrophy: the founding problem stayed live for the whole interval — no sunset was ever declared, and the arrangement terminated with the Mandate itself rather than atrophying into performance while its function survived elsewhere. The theater ratio's terminal rise marks enforcement decay, not completed atrophy: the function did not migrate to another carrier; it lapsed. Had the protective function been fully theatrical from the start, an extraction-dominant reading would fit; the 1939-40 enforcement record shows functional periods that rule that out.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_priority_ordering,
    'This constraint instantiates the dual_obligation_indigenous_rights reading of the balfour_mandate_instruments kernel: the instruments impose equal or superior protective obligations toward the existing Arab population and subordinate the national-home clause to self-determination and minority-protection norms. Which priority ordering between the national-home clause and the protective obligations do the instruments actually encode?',
    'Read the travaux preparatoires of the Mandate drafting, the Covenant Article 22 framing, and the sequence of authoritative glosses (Churchill 1922, Passfield 1930, MacDonald 1931, White Paper 1939) as evidence of which ordering the drafting parties and subsequent authorities took the text to bear.',
    'If the primacy ordering is adopted instead, the victim and beneficiary sets invert: Arab communities become the constrained party and Zionist institutions the subsidized beneficiary, and this story''s epsilon no longer describes the operative arrangement. If interpretive discretion is adopted, the arrangement''s content varies with the administrator''s will and epsilon loses invariance across administrations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_priority_ordering, conceptual, 'Kernel-level ambiguity: which obligation ordering the Mandate instruments encode.').

omega_variable(
    protective_enforcement_sincerity,
    'Were the protective obligations sincerely held and resourced, or maintained declaratorily for League audiences while substance followed the national-home commitment?',
    'Compare Colonial Office internal minutes and resource allocations against public instruments and Permanent Mandates Commission submissions across the interval; measure enforcement outcomes (evictions prevented, quota shortfalls, prosecutions under the transfer regulations) against declared policy.',
    'If declaratory cover, the theater ratio is understated and the arrangement trends toward theatrical maintenance within the Mandate''s life; if sincere but under-resourced, the tangled_rope classification stands with enforcement capacity as the binding constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protective_enforcement_sincerity, empirical, 'Whether the protective function was functional or performative.').

omega_variable(
    beneficiary_granularity_displacement,
    'Did the protective apparatus benefit Arab communities broadly, or chiefly elite landowning interests, while tenant farmers continued to lose tenancy through evasion (nominal eviction, debt foreclosure, sale of tenancy rights)?',
    'Village-level land tenure records and the Hope Simpson enquiry''s cultivation data, tracked against eviction and displacement statistics through the interval.',
    'If gains concentrated in the elite seats while tenants bore continued displacement, the beneficiary structure narrows, effective burden on the tenant seat rises, and the classification drifts toward extraction-dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_granularity_displacement, empirical, 'Distribution of protective gains across Arab social strata.').

omega_variable(
    restriction_bindingness_circumvention,
    'How tightly did the land-transfer zoning and the certificate quota actually bind, given systematic circumvention (front-company acquisitions, open-zone purchasing, undocumented immigration)?',
    'Reconstruct effective land transferred and immigrants admitted outside legal channels versus within them, using land registry gaps, coastguard interception records, and demographic reconstructions.',
    'If circumvention carried most of the flow, the measured burden on the paying seats falls well below the authored value and the arrangement operates closer to a coordination norm than a binding restriction; if interception dominated, the high-burden profile holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restriction_bindingness_circumvention, empirical, 'Effective versus nominal restrictiveness of the apparatus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balfour_dual_obligation_tr_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(balfour_dual_obligation_tr_t0, observed).
narrative_ontology:measurement(balfour_dual_obligation_tr_t4, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 4, 0.22).
narrative_ontology:measurement_basis(balfour_dual_obligation_tr_t4, observed).
narrative_ontology:measurement(balfour_dual_obligation_tr_t8, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 8, 0.26).
narrative_ontology:measurement_basis(balfour_dual_obligation_tr_t8, observed).
narrative_ontology:measurement(balfour_dual_obligation_tr_t12, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 12, 0.36).
narrative_ontology:measurement_basis(balfour_dual_obligation_tr_t12, observed).
narrative_ontology:measurement(balfour_dual_obligation_tr_t16, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 16, 0.33).
narrative_ontology:measurement_basis(balfour_dual_obligation_tr_t16, observed).
narrative_ontology:measurement(balfour_dual_obligation_tr_t20, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(balfour_dual_obligation_tr_t20, observed).
narrative_ontology:measurement(balfour_dual_obligation_tr_t24, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 24, 0.44).
narrative_ontology:measurement_basis(balfour_dual_obligation_tr_t24, observed).
narrative_ontology:measurement(balfour_dual_obligation_tr_t28, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 28, 0.56).
narrative_ontology:measurement_basis(balfour_dual_obligation_tr_t28, observed).

% Extraction over time
narrative_ontology:measurement(balfour_dual_obligation_be_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(balfour_dual_obligation_be_t0, observed).
narrative_ontology:measurement(balfour_dual_obligation_be_t4, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 4, 0.48).
narrative_ontology:measurement_basis(balfour_dual_obligation_be_t4, observed).
narrative_ontology:measurement(balfour_dual_obligation_be_t8, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 8, 0.5).
narrative_ontology:measurement_basis(balfour_dual_obligation_be_t8, observed).
narrative_ontology:measurement(balfour_dual_obligation_be_t12, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 12, 0.57).
narrative_ontology:measurement_basis(balfour_dual_obligation_be_t12, observed).
narrative_ontology:measurement(balfour_dual_obligation_be_t16, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 16, 0.66).
narrative_ontology:measurement_basis(balfour_dual_obligation_be_t16, observed).
narrative_ontology:measurement(balfour_dual_obligation_be_t20, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 20, 0.74).
narrative_ontology:measurement_basis(balfour_dual_obligation_be_t20, observed).
narrative_ontology:measurement(balfour_dual_obligation_be_t24, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 24, 0.67).
narrative_ontology:measurement_basis(balfour_dual_obligation_be_t24, observed).
narrative_ontology:measurement(balfour_dual_obligation_be_t28, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 28, 0.6).
narrative_ontology:measurement_basis(balfour_dual_obligation_be_t28, observed).

% Suppression requirement over time
narrative_ontology:measurement(balfour_dual_obligation_su_t0, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(balfour_dual_obligation_su_t0, observed).
narrative_ontology:measurement(balfour_dual_obligation_su_t4, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 4, 0.38).
narrative_ontology:measurement_basis(balfour_dual_obligation_su_t4, observed).
narrative_ontology:measurement(balfour_dual_obligation_su_t8, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 8, 0.4).
narrative_ontology:measurement_basis(balfour_dual_obligation_su_t8, observed).
narrative_ontology:measurement(balfour_dual_obligation_su_t12, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 12, 0.46).
narrative_ontology:measurement_basis(balfour_dual_obligation_su_t12, observed).
narrative_ontology:measurement(balfour_dual_obligation_su_t16, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 16, 0.62).
narrative_ontology:measurement_basis(balfour_dual_obligation_su_t16, observed).
narrative_ontology:measurement(balfour_dual_obligation_su_t20, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(balfour_dual_obligation_su_t20, observed).
narrative_ontology:measurement(balfour_dual_obligation_su_t24, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 24, 0.55).
narrative_ontology:measurement_basis(balfour_dual_obligation_su_t24, observed).
narrative_ontology:measurement(balfour_dual_obligation_su_t28, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 28, 0.38).
narrative_ontology:measurement_basis(balfour_dual_obligation_su_t28, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Mandate system' covers three structurally distinct constraints sharing one kernel of texts: this reading (protective obligations equal or superior, national home subordinate), the primacy reading (demographic and territorial transformation toward sovereignty as the instruments' directive), and the discretionary reading (the administration's unreviewable choice as the operative rule). Each carries its own epsilon, beneficiary/victim structure, and classification; they are linked here as a constraint family. The discretionary reading is upstream — allocation of interpretive authority determines which substantive reading governs — while this reading and the primacy reading are downstream rivals whose victim and beneficiary sets invert relative to each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
