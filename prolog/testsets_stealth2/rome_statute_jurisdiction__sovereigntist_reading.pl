% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__sovereigntist_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__sovereigntist_reading
 *   human_readable: Rome Statute Jurisdiction — Sovereigntist Reading (Consent-Gated)
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   The Rome Statute's jurisdictional architecture is a contested kernel:
 *   three readings assign it different legal content. This file instantiates
 *   the sovereigntist reading ONLY — the Statute as a conditional framework
 *   in which jurisdiction exists strictly where consent places it
 *   (ratification, Article 12(3) declaration, or Security Council referral),
 *   non-party nationals are immune absent referral, national courts retain
 *   primary authority, and complementarity operates as deference rather than
 *   override. Per the epsilon-invariance principle this reading is one
 *   constraint with one stable, reading-indexed epsilon over the standing
 *   consent-gated arrangement; the universalist and hybrid readings are
 *   separate stories with their own epsilon values, linked via
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   nonparty_great_powers: Primary beneficiary (powerful/arbitrage) —
 *   shielded by a gate they never accepted, reachable only through a council
 *   they can veto - victims_of_nonparty_atrocities: Primary target
 *   (powerless/trapped) — denied remedy when perpetrators' states stayed
 *   outside - party_state_defendants: Secondary target (powerless/trapped) —
 *   bear exposure their non-party counterparts escape -
 *   assembly_of_states_parties: Agenda setter (institutional/constrained) —
 *   administers the consent architecture - icc_pretrial_chambers: Agenda
 *   setter (institutional/trapped) — sets the operative reading while bound
 *   by the text it interprets - small_states_parties: Dual-positioned
 *   (moderate/constrained) — bought protection with exposure -
 *   national_judiciaries_of_party_states: Beneficiary
 *   (institutional/identity_locked) — primacy confirmed by deference -
 *   universalist_advocates: Excluded voice (organized/mobile) — object
 *   without leverage - international_law_scholars: Analytical observer
 *   (analytical/analytical) — maps the contest
 *
 * KEY AGENTS:
 *   - nonparty_great_powers: Primary beneficiary (powerful/arbitrage) — nationals shielded by the consent gate they never accepted
 *   - victims_of_nonparty_atrocities: Primary target (powerless/trapped) — no forum when perpetrators' states stayed outside
 *   - party_state_defendants: Secondary target (powerless/trapped) — asymmetric exposure relative to non-party counterparts
 *   - assembly_of_states_parties: Agenda setter (institutional/constrained) — administers amendments, elections, budget
 *   - icc_pretrial_chambers: Agenda setter (institutional/trapped) — adjudicates the consent boundary case by case
 *   - small_states_parties: Dual-positioned beneficiary/payer (moderate/constrained) — protection purchased with exposure
 *   - national_judiciaries_of_party_states: Beneficiary (institutional/identity_locked) — primary authority preserved by deference
 *   - universalist_advocates: Excluded (organized/mobile) — contest the gate without procedural leverage
 *   - international_law_scholars: Analytical observer (analytical/analytical) — traces the boundary's movement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.22).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.2).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "Rome Statute Jurisdiction — Sovereigntist Reading (Consent-Gated)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__sovereigntist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, '3a229604-6a98-4b33-956d-e0fe60cfce9b').
narrative_ontology:cs_kernel_codification('3a229604-6a98-4b33-956d-e0fe60cfce9b', fixed_text).
narrative_ontology:cs_authority_grounding('3a229604-6a98-4b33-956d-e0fe60cfce9b', lineage).
narrative_ontology:cs_interpretation_layer_present('3a229604-6a98-4b33-956d-e0fe60cfce9b').
narrative_ontology:cs_reading_relation('3a229604-6a98-4b33-956d-e0fe60cfce9b', rome_statute_jurisdiction__universalist_reading, forecloses).
narrative_ontology:cs_reading_relation('3a229604-6a98-4b33-956d-e0fe60cfce9b', rome_statute_jurisdiction__hybrid_complementarity_reading, forecloses).
narrative_ontology:cs_axiom('3a229604-6a98-4b33-956d-e0fe60cfce9b', foundational, strict_consent_gates_icc_jurisdiction).
narrative_ontology:cs_axiom_status(strict_consent_gates_icc_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('3a229604-6a98-4b33-956d-e0fe60cfce9b', strict_consent_gates_icc_jurisdiction, conventional).
narrative_ontology:cs_axiom('3a229604-6a98-4b33-956d-e0fe60cfce9b', foundational, national_courts_hold_primary_authority).
narrative_ontology:cs_axiom_status(national_courts_hold_primary_authority, holdable).
narrative_ontology:cs_axiom_grounding('3a229604-6a98-4b33-956d-e0fe60cfce9b', national_courts_hold_primary_authority, conventional).
narrative_ontology:cs_reference_frame('3a229604-6a98-4b33-956d-e0fe60cfce9b', sovereign_consent_jurisdiction_framework).
narrative_ontology:cs_drift_state('3a229604-6a98-4b33-956d-e0fe60cfce9b', contemporary_expansive_jurisprudence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3a229604-6a98-4b33-956d-e0fe60cfce9b', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, nonparty_great_powers).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, small_states_parties).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, national_judiciaries_of_party_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, victims_of_nonparty_atrocities).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, party_state_defendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, icc_pretrial_chambers).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, small_states_parties).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, pacta_tertiis_principle).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__sovereigntist_reading, sovereign_equality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The treaty body of ratifying states. It adopts amendments, elects judges and prosecutors, and sets the budget; any widening of the court's reach requires its supermajorities. Several of its members have withdrawn or threatened withdrawal when rulings strained what they had consented to. Leaving means denouncing the treaty — procedurally available, as Burundi and the Philippines showed, but diplomatically costly and forfeiting the seat at the table.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, assembly_of_states_parties, agenda_setter,
    institutional, generational, constrained, global).

% The Court's judges decide jurisdiction and admissibility case by case, and through those rulings they set the operative reading of the consent rules. They are bound by the very text they interpret — they cannot step outside the consent framework — and each expansive ruling spends legitimacy with the states whose cooperation and contributions sustain the institution.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, icc_pretrial_chambers, agenda_setter,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, icc_pretrial_chambers, payer).

% Major military powers that never ratified. Their officials and soldiers cannot be prosecuted by the Court for acts of their own nationals on their own territory unless the Security Council refers a situation — which any of them can veto. They have signed bilateral non-surrender agreements with dozens of states and lobby against expansion. They pay nothing into a system whose boundaries they benefit from remaining outside.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, nonparty_great_powers, beneficiary,
    powerful, generational, arbitrage, global).

% States that ratified seeking a backstop against atrocities on their territory — they acquired a deterrent they could not build alone. The price: their nationals fall within the Court's reach while the great powers' do not, an asymmetry visible in the docket's composition. Withdrawal remains available but carries diplomatic cost and strips away the protection they joined to obtain.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, small_states_parties, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__sovereigntist_reading, small_states_parties, payer).

% Domestic courts get first claim on prosecuting atrocity crimes by their own nationals; the international court acts only where they are unable or unwilling. Every ruling affirming deference confirms their primacy. Their dockets, budgets, and professional self-conception are built around being the primary forum, and they would resist any interpretive move that converted deference into supervision.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, national_judiciaries_of_party_states, beneficiary,
    institutional, generational, identity_locked, national).

% Survivors of atrocities committed by nationals of states outside the treaty. When the perpetrator's state never joined and the Security Council is blocked, no international forum will hear the case. Their access to justice turns on an accession decision made in capitals they cannot influence, and the framework gives them no procedural seat of their own.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, victims_of_nonparty_atrocities, payer,
    powerless, biographical, trapped, regional).

% Nationals of ratifying states who face prosecution that identically situated nationals of non-party states escape. Soldiers and officials of contributing states carry legal exposure their counterparts from non-member states do not — an asymmetry that shapes peacekeeping deployment decisions and feeds domestic political pressure on governments to withdraw.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, party_state_defendants, payer,
    powerless, immediate, trapped, local).

% Human rights organizations and legal campaigners who hold that atrocity accountability should not depend on the perpetrator's state having consented. They testify, file amicus briefs, and campaign for broader readings, but hold no vote in the Assembly and no seat in the referral process; the amendment rules give them voice without leverage.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, universalist_advocates, excluded,
    organized, generational, mobile, global).

% Academic interpreters who map the competing readings of the statute and trace how successive rulings move the consent boundary. They collect no rents and bear no exposure; their analyses supply the vocabulary in which the other seats argue.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__sovereigntist_reading, nonparty_great_powers).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__sovereigntist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Makes a permanent international criminal court achievable at all: by guaranteeing the court reaches only those who accept it, the consent rule solved the collective-action problem that defeated earlier attempts — sovereign states would not build a prosecutor they could not bound. It also allocates jurisdictional competence between the national and international levels, keeping primary authority domestic.
% TRANSFER_FUNCTION: Moves prosecutorial authority over atrocity crimes from wherever a universal forum would have taken it back to national control. Concretely, it moves legal exposure onto nationals of consenting states and immunity onto nationals of non-consenting ones, with Security Council referral as the sole bypass.
% ABSENT_VOICES: Victims of atrocities by non-party-state nationals and their advocates have no procedural seat: the framework allocates voice by accession, so those harmed by outsiders' perpetrators are represented by no one in the Assembly or the referral process. Universalist campaigning organizations speak publicly but hold no vote; their objection — that accountability should not turn on the perpetrator's passport — enters the record only as persuasion.
% DISAPPEARANCE_RATIONALE: If the consent gate vanished overnight — jurisdiction becoming universal — non-party powers would face immediate exposure of officials and troops, bilateral immunity agreements would proliferate, several states parties would withdraw rather than accept symmetric exposure, Security Council referral politics would invert, and the Court's docket and funding would reorganize around the new reach. The arrangements of every named seat depend on the gate.
% FOUNDING_PROBLEM: Post-Nuremberg impunity: genocide, war crimes, and crimes against humanity were prosecuted only when ad hoc tribunals were politically assembled (Yugoslavia, Rwanda), leaving every other atrocity untouched. A permanent court had been proposed since the 1950s but stalled for decades because states would not accept a prosecutor beyond their control.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: UN Secretary-General reports and Security Council debates establishing the ad hoc tribunals document the impunity gap; the ICTY/ICTR legacy and cost records show why a permanent court was sought; scholarly consensus across interpretive traditions accepts the historical founding problem even where readings of the solution diverge. Notably, the chief beneficiaries of this reading — the non-party great powers — do not attest it; they dispute the court's value altogether, which strengthens rather than weakens the genealogy's independence.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__sovereigntist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__sovereigntist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__sovereigntist_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).
:- end_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   All values are reading-indexed: assessed by the sovereigntist reading's own lights, the consent-gated arrangement is predominantly protective rather than extractive, hence low epsilon (0.22) — the residual reflects the acknowledged asymmetry in which small consenting states bind themselves while great non-parties ride free. Suppression (0.20) is authored as a raw structural property, unscaled by power or scope: the gate binds the Court, not states, and no participant is coerced into membership. Theater (0.24) captures ritual accretion — Assembly ceremony, complementarity rhetoric — around a functional core. Accessibility_collapse (0.30) is low because alternatives remain fully open: national prosecution, ad hoc mechanisms, diplomacy, universal-jurisdiction statutes in third states. Resistance (0.58) is substantial because the reading is actively contested by universalist advocacy, NGO campaigns, and a scholarly literature that treats the consent boundary as the central betrayal of the Statute's promise. The temporal series run on one shared grid (1998/2002/2008/2014/2019/2026) with every tracked metric authored at every point: extractiveness creeps up as the party/non-party asymmetry manifests in the docket, theater accumulates slowly, and suppression_requirement FALLS steadily — the active force needed to police the consent boundary has decayed as expansive jurisprudence normalized readings the 2002-era gatekeepers would have challenged. The suppression_requirement series is authored deliberately: this story's dynamic is enforcement decay of the boundary, not stability.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute sharply different experiences from identical structural data. The trapped payer seats (victims of non-party atrocities, party-state defendants) sit near the full-target end of directionality: the gate is the thing between them and a forum, so their computed effective extraction approaches the full base rate. The arbitrage beneficiary seat (non-party great powers) sits near the full-beneficiary end: the gate subsidizes them at zero cost, and their exit option — they were never inside — makes the arrangement nearly costless. The agenda-setter seats sit near symmetric: the Assembly and the chambers both administer the gate and bear its legitimacy costs. The engine derives these divergences from the declared roles, power atoms, and exit options; the authored rope claim reflects this reading's own structural interpretation and does not adjudicate the per-seat results.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: non-party great powers (arbitrage exit, never bound) derive nearest the beneficiary pole; national judiciaries (identity_locked — their institutional self-conception is constituted by retained primacy) derive low despite formal exposure to reinterpretation; small states parties derive moderately low, discounted by their secondary payer position. Victim declarations drive high directionality: victims of non-party atrocities and party-state defendants are trapped, powerless, and named in the victims array, placing them near the full-target pole. No directionality overrides are used: the derivation chain from beneficiary/victim declarations plus exit options already separates the seats correctly, and the override surface is keyed by power atom, which would misfire across the distinct institutional agents sharing the 'institutional' atom here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — post-Nuremberg impunity reachable only through politically assembled ad hoc tribunals — is still live, so no mandatrophy resolution is declared and no sunset applies. The classification discipline guards against two opposite errors. Reading the consent gate as pure extraction (a snare verdict) would erase the reason 125 states joined: the coordination function is real, and without the gate there would likely be no permanent court at all. Reading it as frictionless coordination would ignore the identifiable seats that pay through the same structure — the trapped victims and the asymmetrically exposed defendants — whose existence is exactly what the universalist sibling story isolates as high epsilon. The R5 mismatch consumer finds status=live crossed with verdict=world_rearranges: no zombie flag; the arrangement's persistence tracks a problem that still exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_index_epsilon_divergence,
    'This constraint is one reading of the rome_statute_jurisdiction kernel — what would the sibling readings change about the victim set, the beneficiary structure, and the measured extraction?',
    'Author the sibling files (rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading) over the same referent and compare their epsilon values, declared victim sets, and computed per-seat types; the contest resolves across stories, never by hedging within this one.',
    'The universalist sibling authors high epsilon (impunity-for-the-powerful as the arrangement''s output) and should compute sharply extractive payer seats; the hybrid lands between. Cross-reading comparison is the only admissible resolution path for the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_index_epsilon_divergence, conceptual, 'Committer structure: this story is the sovereigntist member of a three-reading kernel; sibling readings instantiate different constraints with different epsilon.').

omega_variable(
    territorial_hook_doctrine,
    'Does the consent gate admit territorial jurisdiction over non-party nationals whose crimes touch party territory (the Bangladesh/Myanmar line of decisions), or does strict consent immunize them regardless of where the crime occurred?',
    'Appellate consolidation of the territorial-jurisdiction decisions, Assembly reactions, and state practice on Article 12(3) declarations over the coming case cycle.',
    'If the territorial hook stands, the immunity premise of this reading narrows and its measured extraction rises toward the hybrid sibling''s profile; if rejected, the strict reading consolidates and the falling suppression series reads as successful defense of the boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_hook_doctrine, empirical, 'Scope of the territorial exception to consent-based immunity — the sharpest doctrinal pressure on this reading.').

omega_variable(
    complementarity_valence,
    'Does complementarity operate as deference (national proceedings presumed genuine unless demonstrably sham) or as supervision (the Court actively audits genuineness)?',
    'Admissibility outcome patterns in situations with contested national proceedings; comparative analysis of willingness findings across chambers and years.',
    'Deference keeps this reading''s coordination function intact and its payer seats few; supervision converts the Court into a reviewer of national good faith, raising the extraction the trapped seats compute and pulling the structure toward the hybrid sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_valence, conceptual, 'Whether the complementarity mechanism is deferential or supervisory in operation.').

omega_variable(
    consent_gate_enforcement_decay,
    'Does the falling suppression_requirement series record erosion of the consent boundary (the gate failing under jurisprudential and political pressure) or its normalization into an uncontested baseline (the gate succeeding so thoroughly it no longer needs policing)?',
    'Track withdrawal threats, Article 98 agreement churn, Assembly votes on jurisdiction-expanding proposals, and non-party funding responses across the interval''s tail.',
    'Erosion supports reclassification pressure toward the hybrid sibling''s structure and eventual elevation of this reading''s epsilon; normalization stabilizes the sovereigntist reading as the settled meaning of the text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_gate_enforcement_decay, empirical, 'Competing interpretations of the declining enforcement-intensity trajectory authored in the temporal series.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 1998, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 1998, 0.08).
narrative_ontology:measurement_basis(rome_tr_t1998, observed).
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2002, 0.12).
narrative_ontology:measurement_basis(rome_tr_t2002, observed).
narrative_ontology:measurement(rome_tr_t2008, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement_basis(rome_tr_t2008, observed).
narrative_ontology:measurement(rome_tr_t2014, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2014, 0.19).
narrative_ontology:measurement_basis(rome_tr_t2014, observed).
narrative_ontology:measurement(rome_tr_t2019, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2019, 0.22).
narrative_ontology:measurement_basis(rome_tr_t2019, observed).
narrative_ontology:measurement(rome_tr_t2026, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2026, 0.24).
narrative_ontology:measurement_basis(rome_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 1998, 0.1).
narrative_ontology:measurement_basis(rome_be_t1998, observed).
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2002, 0.14).
narrative_ontology:measurement_basis(rome_be_t2002, observed).
narrative_ontology:measurement(rome_be_t2008, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2008, 0.16).
narrative_ontology:measurement_basis(rome_be_t2008, observed).
narrative_ontology:measurement(rome_be_t2014, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2014, 0.19).
narrative_ontology:measurement_basis(rome_be_t2014, observed).
narrative_ontology:measurement(rome_be_t2019, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2019, 0.21).
narrative_ontology:measurement_basis(rome_be_t2019, observed).
narrative_ontology:measurement(rome_be_t2026, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2026, 0.22).
narrative_ontology:measurement_basis(rome_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 1998, 0.55).
narrative_ontology:measurement_basis(rome_su_t1998, observed).
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2002, 0.5).
narrative_ontology:measurement_basis(rome_su_t2002, observed).
narrative_ontology:measurement(rome_su_t2008, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2008, 0.44).
narrative_ontology:measurement_basis(rome_su_t2008, observed).
narrative_ontology:measurement(rome_su_t2014, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2014, 0.36).
narrative_ontology:measurement_basis(rome_su_t2014, observed).
narrative_ontology:measurement(rome_su_t2019, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2019, 0.28).
narrative_ontology:measurement_basis(rome_su_t2019, observed).
narrative_ontology:measurement(rome_su_t2026, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2026, 0.2).
narrative_ontology:measurement_basis(rome_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__sovereigntist_reading, resource_allocation).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'ICC jurisdiction under the Rome Statute' decomposes into three structurally distinct constraints — one per reading of the kernel. The sovereigntist reading (this file) authors low reading-indexed epsilon over the consent-gated arrangement; the universalist sibling authors high epsilon over the same referent (identifying non-party impunity as the arrangement's product); the hybrid sibling sits between, admitting territorial hooks while retaining complementarity. The upstream/downstream structure runs through shared text: each sibling cites the same articles but assigns them opposite operative content, so the family is linked by affects_constraints edges in all three files rather than by any single authoritative reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
