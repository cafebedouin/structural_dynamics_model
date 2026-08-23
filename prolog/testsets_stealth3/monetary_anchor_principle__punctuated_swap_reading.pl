% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__punctuated_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__punctuated_swap_reading, []).

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
 *   constraint_id: monetary_anchor_principle__punctuated_swap_reading
 *   human_readable: Dollar-Gold Anchor Regime as Discrete Institutional Choice (Punctuated-Swap Reading)
 *   domain: monetary economics/political economy/international finance
 *
 * SUMMARY:
 *   On August 15, 1971 the United States suspended dollar-gold
 *   convertibility, imposed a temporary import surcharge, and froze wages and
 *   prices — the Nixon Shock. This story instantiates the
 *   punctuated_swap_reading of the monetary_anchor_principle kernel: the
 *   transition was a single discrete swap between regimes, chosen and
 *   executed by the US authorities, not an inevitable structural collapse.
 *   The constraint under assessment is the dollar-gold anchor arrangement as
 *   it stood and ended. Per the epsilon-invariance rule, the sibling readings
 *   (overdetermined composite, Triffin inevitability) are separate
 *   constraints in the same family, linked by network edges, not folded into
 *   this one. The claim/metric gap is deliberate: the reading CLAIMS a
 *   coordination device chosen and revisable in principle, while the authored
 *   metrics record the asymmetric issuer privilege and the crystallized 1971
 *   transfer — the engine measures that divergence rather than the author
 *   reconciling it.
 *
 * KEY AGENTS:
 *   - us_fiscal_authorities: Agenda setter and principal beneficiary (institutional/arbitrage) — administered the dollar-gold regime and alone held the power to rewrite its terms on August 15, 1971
 *   - foreign_dollar_holders: Primary payer (organized/trapped) — accumulated dollar claims against a shrinking gold stock and bore the crystallized devaluation loss when the window closed
 *   - export_led_economies: Secondary beneficiary and secondary payer (powerful/constrained) — traded under anchor-stabilized rates while absorbing US inflation, pressured revaluations, and the 1971-73 reserve markdowns
 *   - imf_par_value_administration: Co-administrator without agenda control (institutional/identity-locked) — ran par-value surveillance; lost its founding function at the swap and reinvented by 1976
 *   - gold_producers: Excluded voice (moderate/constrained) — demonetization decided without their seat at the table
 *   - monetary_historians: Analytical observer (analytical/analytical) — reconstructs the decision record and hosts the kernel's competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, 0.55).
domain_priors:suppression_score(monetary_anchor_principle__punctuated_swap_reading, 0.3).
domain_priors:theater_ratio(monetary_anchor_principle__punctuated_swap_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__punctuated_swap_reading, rope).
narrative_ontology:human_readable(monetary_anchor_principle__punctuated_swap_reading, "Dollar-Gold Anchor Regime as Discrete Institutional Choice (Punctuated-Swap Reading)").
narrative_ontology:topic_domain(monetary_anchor_principle__punctuated_swap_reading, "monetary economics/political economy/international finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__punctuated_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, '9c91e8c1-a460-44d8-a709-49f394253ec4').
narrative_ontology:cs_kernel_codification('9c91e8c1-a460-44d8-a709-49f394253ec4', formalized).
narrative_ontology:cs_authority_grounding('9c91e8c1-a460-44d8-a709-49f394253ec4', extraction).
narrative_ontology:cs_interpretation_layer_present('9c91e8c1-a460-44d8-a709-49f394253ec4').
narrative_ontology:cs_reading_relation('9c91e8c1-a460-44d8-a709-49f394253ec4', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c91e8c1-a460-44d8-a709-49f394253ec4', monetary_anchor_principle__triffin_inevitability_reading, coexists_with).
narrative_ontology:cs_axiom('9c91e8c1-a460-44d8-a709-49f394253ec4', foundational, anchor_regime_was_discretionary_instrument).
narrative_ontology:cs_axiom_status(anchor_regime_was_discretionary_instrument, holdable).
narrative_ontology:cs_axiom_grounding('9c91e8c1-a460-44d8-a709-49f394253ec4', anchor_regime_was_discretionary_instrument, empirically_contingent).
narrative_ontology:cs_axiom('9c91e8c1-a460-44d8-a709-49f394253ec4', secondary, defection_responsibility_is_attributable).
narrative_ontology:cs_axiom_status(defection_responsibility_is_attributable, holdable).
narrative_ontology:cs_axiom_grounding('9c91e8c1-a460-44d8-a709-49f394253ec4', defection_responsibility_is_attributable, deontological).
narrative_ontology:cs_reference_frame('9c91e8c1-a460-44d8-a709-49f394253ec4', institutionally_chosen_gold_anchor).
narrative_ontology:cs_drift_state('9c91e8c1-a460-44d8-a709-49f394253ec4', contemporary_fiat_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('9c91e8c1-a460-44d8-a709-49f394253ec4', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authorities).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, export_led_economies).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, export_led_economies).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__punctuated_swap_reading, monetary_regime_choice_doctrine).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__punctuated_swap_reading, national_monetary_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Nixon administration, Treasury, and Federal Reserve administered the dollar-gold system and collected seigniorage and macroeconomic room from issuing the reserve currency. On August 15, 1971 they suspended convertibility, imposed a temporary import surcharge, and froze wages and prices in a single weekend package. Their exit from the arrangement was never leaving it but rewriting its terms unilaterally, which is what the discrete-swap claim turns on; the electoral calendar of 1972 shaped the timing.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authorities, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authorities, beneficiary).

% Foreign central banks, treasuries, and private holders accumulated dollar claims against a US gold stock that fell from roughly 20,000 tonnes in the late 1950s to under 9,000 by 1971 while external dollar liabilities multiplied. Each holder could convert at $35/oz only so long as others did not; mass conversion would exhaust the gold stock and devalue every remaining claim, so the individually rational move was to hold. When the window closed they bore the realized loss: the official gold price was raised twice by 1973 and the dollar's market gold value fell by roughly two-thirds within the decade. G10 and BIS machinery gave them voice but no agenda control.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders, payer,
    organized, generational, trapped, global).

% Germany, Japan, and other export-led economies rebuilt trade under exchange rates pegged through the dollar anchor, which underwrote their growth model. They also absorbed US inflation through their pegs, came under direct American pressure to revalue (the 1969 and 1971 German revaluations followed explicit threats over troop offsets), and saw their dollar reserves marked down in 1971 and 1973. Unilateral floating, as Germany attempted in May 1971, was available but costly in trade terms and politically fraught with Washington.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, export_led_economies, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, export_led_economies, payer).

% The IMF secretariat and its committees administered par values, ran surveillance, and processed parity changes; its founding identity was the par-value system itself. It had voice in the regime's operation but learned of the gold-window closure after the fact. Between 1971 and 1976 it administered rules that no longer described practice, until the Second Amendment formally abandoned par values and the institution reinvented itself around surveillance of floating and SDR management.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, imf_par_value_administration, agenda_setter,
    institutional, generational, identity_locked, global).

% South African, Soviet, Canadian, and other gold producers, and the mining industry around them, would have objected to demonetization: the official $35 floor was the monetary demand underpinning their product. They held no seat in the G10 or IMF deliberations that ended it. The 1968 two-tier arrangement had already split their market from the official one, and the 1971 closure removed the official price entirely.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, gold_producers, excluded,
    moderate, generational, constrained, global).

% Monetary economists and economic historians — Triffin and Rueff in their own day, Eichengreen, Bordo, and the archival literature after — reconstruct the transition from decision records, gold-flow data, and central bank archives. They see the full structure: the overhang arithmetic, the option set as the principals understood it, and the counterfactual debates on which the kernel's competing readings turn.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authorities).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__punctuated_swap_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The dollar-gold anchor gave the non-communist trading world a single nominal anchor and a liquidity source: countries pegged to the dollar, the dollar pegged to gold at $35/oz, and the United States supplying reserve assets through its balance-of-payments position. This solved the interwar problem — competitive devaluation and liquidity shortage — with one fix point instead of per-country gold hoarding, and let postwar Europe and Japan rebuild trade under predictable rates.
% TRANSFER_FUNCTION: Moved seigniorage and macroeconomic autonomy to the United States: foreigners held dollar claims, financing US deficits (notably Vietnam-era) at below-market cost while the US alone decided when, and whether, to honor convertibility. Moved devaluation risk to foreign holders, whose claims were claims on a shrinking gold stock; the 1971 closure converted that risk into realized loss (official gold price from $35 to $42.22 by 1973; market value several hundred dollars by decade's end).
% ABSENT_VOICES: Gold producers and gold-backed savers had no seat when demonetization was decided. Foreign holders were present through G10 machinery but without agenda control, and their collective-action position meant each preferred that someone else test the window. Dissent inside the United States (Fed chairman Burns opposed closure) was overruled by the executive. The Smithsonian announcement was presented as a multilateral agreement; the decision itself had been taken unilaterally.
% DISAPPEARANCE_RATIONALE: The trading world's monetary arrangements were organized around the anchor: par values, reserve composition, invoicing conventions, and the IMF's rulebook all presupposed it. When the swap came, everything downstream rearranged — generalized floating by 1973, a fiat dollar standard with no convertibility check, the inflation of the 1970s, petrodollar recycling, and the IMF's forced reinvention. Nothing snapped back to a pre-anchor default; a new regime had to be constructed.
% FOUNDING_PROBLEM: The regime was designed at Bretton Woods in 1944 to solve interwar monetary chaos: competitive devaluations, beggar-thy-neighbor trade policy, collapsed gold exchange standards, and the liquidity shortage that choked reconstruction trade. The anchor was the chosen instrument for that problem — which is why, under this reading, its termination is an institutional choice about a still-live problem rather than the problem's disappearance.
% FOUNDING_PROBLEM_CORROBORATION: The anchor problem's persistence after 1971 is attested from outside the benefiting parties: the Committee of Twenty reform negotiations (1972-74) and the Jamaica Accords (1976) show the same states that lost the gold anchor immediately building replacement anchor arrangements; Bundesbank and BIS archival records document holders' active search for alternatives; and the monetary-history literature treats anchor provision as a continuing unsolved problem rather than a closed one. The US authorities' contemporaneous justification (gold drain, speculative attack) corroborates that the swap answered a live pressure, not a dead one.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__punctuated_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__punctuated_swap_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__punctuated_swap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monetary_anchor_principle__punctuated_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__punctuated_swap_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.55 at interval end) because the anchor regime combined genuine coordination value with concentrated issuer privilege; the 1971 peak of 0.68 is the overhang crystallizing into realized loss. Suppression (0.30 end-state) traces the enforcement arc: consent plus capital controls in the 1950s, the Gold Pool and credit restraints by the mid-1960s, mandatory controls and two-tier gold defense by 1968, the surcharge and wage-price freeze at the 1971 peak, then rapid relaxation once floating removed the window there was left to defend. Theater (0.35 end-state) peaked at 0.55 in 1971: by then the $35/oz promise was maintained only among official holders while the private market priced gold far above it, so the convertibility core had become performative before it was closed. All three series run on one shared grid (1944-1980, eight points) so every metric is authored at every examined time point. The trajectory is a punctuated arc, not a cycle: long strain, a discrete break, a new plateau.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (US authorities), the regime was an instrument: built by choice, strained by others' claims, set aside when its costs exceeded its benefits — the swap is evidence of sovereignty, and the coordination claim is their self-description. From the payer seat (foreign holders), the same record reads as a promise held just long enough for the overhang to become unrecoverable, then closed — coordination followed by expropriation. The IMF seat experienced the event as the death of its founding function. The engine computes these per-seat classifications from the structural data; the divergence between the authored coordination claim and the payer-seat computation is the datum this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The US fiscal-monetary authorities are declared beneficiaries and are the agenda setters: they collected seigniorage and policy autonomy and alone could redefine the terms — derivation places them near the beneficiary end, amplified by arbitrage-grade exit (they exited by rewriting, not leaving). Foreign dollar holders are declared victims: they bore the transfer, and their exit was closed by the collective-action structure of the overhang (each holder's conversion threatened all holders' claims), placing them near the full-target end. Export-led economies sit mixed: genuine coordination benefit from stable rates against real costs from absorbed US inflation, forced revaluations, and the reserve markdowns. Gold producers are excluded rather than coordinated — their exclusion is part of what demonetization required. Scope is global, which the engine reflects in its effective-extraction scaling.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is keeping three mislabels apart. Reading the regime as pure extraction would erase the real coordination function (postwar trade reconstruction under stable rates) that even its payers used; reading it as natural structure would erase the choice — the whole punctuated-swap point is that nothing structural forced the August 15 form. The mandatrophy risk in this family sits one seat over: the IMF's par-value administration between 1971 and 1976 was a function outliving its object, maintained until the Second Amendment admitted the regime was gone. That residue belongs to the institution's story, not to the anchor constraint itself, which terminated cleanly by decision rather than decaying inertially. The coordination claim plus the declared payer keeps the analysis honest on both sides: a working coordination arrangement with an asymmetric, chosen, and attributable ending.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story is one reading of the monetary_anchor_principle kernel — the punctuated_swap_reading, which holds the 1971 transition was a discrete institutional choice. What would adopting a sibling reading change structurally?',
    'Cross-reading comparison within the kernel family: the overdetermined_composite_reading redistributes causation across many contributing pressures and diffuses responsibility; the triffin_inevitability_reading relocates causation to the reserve-issuer dilemma and removes choice. Adopting a sibling would change the beneficiary/victim map (diffuse versus concentrated) and delete the reversibility premise.',
    'If a sibling reading were adopted, this constraint''s attribution of the transition to the choosing US authorities would dissolve into structural or composite attribution, and the payer seat''s claim would weaken from ''chosen act with attributable responsibility'' to ''structural outcome''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this constraint is one reading of the monetary-anchor kernel; sibling readings would reattribute causation and responsibility.').

omega_variable(
    counterfactual_defensibility,
    'Could the United States have defended gold convertibility through the early 1970s at politically feasible cost (contraction, devalue-then-restore, capital controls alone), or were the August 1971 options exhausted?',
    'Counterfactual economic history: reconstruct feasible policy paths from the 1968-71 record (gold flows, monetary aggregates, the electoral calendar) and test whether any preserved convertibility; archival decision-record analysis of which options the principals believed were open.',
    'If defensible paths existed, the swap is a genuine choice and this reading''s reversibility premise holds; if not, the ''choice'' was forced and the story converges toward the composite and Triffin siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_defensibility, empirical, 'Whether the discrete swap was a free choice or a forced move — the empirical hinge of this reading.').

omega_variable(
    regime_event_referent_split,
    'Does this story''s epsilon referent — the anchor arrangement across its arc, including the termination act — hide two structurally distinct constraints: the standing regime (a coordination device with issuer privilege) and the termination event (a one-time transfer realized at closure)?',
    'Epsilon-invariance decomposition test: author the pre-1971 regime and the 1971 closure as separate stories; if the regime alone certifies as low-extraction coordination while the closure event certifies as a high-extraction transfer, split the family and link via network edges.',
    'Splitting would give the standing regime an epsilon near 0.30 and the termination event an epsilon near 0.70, replacing this story''s arc-averaged 0.55; the coordination claim would then apply cleanly to the regime story while the event story would classify on its own.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_event_referent_split, conceptual, 'Whether the regime and its termination are one constraint or a two-story family.').

omega_variable(
    holder_compensation_bargain,
    'Did foreign holders'' continued dollar accumulation reflect a closed exit, or an implicit bargain (US security commitments, market access, troop-offset agreements) that compensated them for carrying devaluation risk?',
    'Archival analysis of the 1960s US-Germany troop-offset negotiations, BIS and G10 deliberations, and foreign central bank reserve-policy records: whether holders priced an explicit or implicit compensation stream against the risk they carried.',
    'If a compensation bargain existed, the payer seat''s net loss shrinks and the arrangement looks closer to consensual coordination with a bad end-state; if not, the expropriation framing stands at full weight and the payer seat''s classification hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(holder_compensation_bargain, empirical, 'Whether the payers were uncompensated or paid in security services — the victim-structure question.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__punctuated_swap_reading, 1944, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1944, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(mone_tr_t1950, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(mone_tr_t1958, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1958, 0.18).
narrative_ontology:measurement(mone_tr_t1965, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(mone_tr_t1968, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1968, 0.45).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1971, 0.55).
narrative_ontology:measurement(mone_tr_t1974, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1974, 0.4).
narrative_ontology:measurement(mone_tr_t1980, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1980, 0.35).

% Extraction over time
narrative_ontology:measurement(mone_be_t1944, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1944, 0.25).
narrative_ontology:measurement(mone_be_t1950, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement(mone_be_t1958, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1958, 0.32).
narrative_ontology:measurement(mone_be_t1965, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1965, 0.45).
narrative_ontology:measurement(mone_be_t1968, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1968, 0.55).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1971, 0.68).
narrative_ontology:measurement(mone_be_t1974, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1974, 0.6).
narrative_ontology:measurement(mone_be_t1980, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1980, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1944, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1944, 0.2).
narrative_ontology:measurement(mone_su_t1950, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1950, 0.22).
narrative_ontology:measurement(mone_su_t1958, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1958, 0.3).
narrative_ontology:measurement(mone_su_t1965, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1965, 0.48).
narrative_ontology:measurement(mone_su_t1968, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1968, 0.6).
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1971, 0.7).
narrative_ontology:measurement(mone_su_t1974, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1974, 0.35).
narrative_ontology:measurement(mone_su_t1980, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1980, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__punctuated_swap_reading, resource_allocation).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__overdetermined_composite_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'why did Bretton Woods end' decomposes into three structurally distinct claims with different epsilon attributions, different beneficiary/victim maps, and different counterfactual commitments. This story carries the discrete-choice attribution: concentrated causation, concentrated receipt (US authorities), reversibility in principle. The overdetermined_composite_reading distributes causation and diffuses the payer's target status; the triffin_inevitability_reading removes choice entirely and converts the payer's loss into structural cost. The upstream empirical record (decision archives, gold-flow data) is shared; each reading weights it differently. Family members link via affects_constraints; epsilon differences are documented in each file's commentary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
